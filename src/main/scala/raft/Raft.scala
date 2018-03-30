package raft

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import raft.State.{Candidate, Follower, Leader}

import scala.concurrent.duration._

/**
  * Created by <yuemenglong@126.com> on 2018/3/28.
  */

class Raft {

}

class State

object State {

  case object Leader extends State

  case object Follower extends State

  case object Candidate extends State

}

class UnreachableException extends RuntimeException("Unreachable")

object Context {
  val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = akka.util.Timeout(1 second)

  val maxHeartBeat = 8
  val maxTimeout = 3

  def props[T <: Actor](clazz: Class[T], args: Any*): ActorRef = {
    //noinspection AppropriateActorConstructorNotFound
    system.actorOf(Props(clazz, args: _*))
  }
}

object Servers {
  private var servers: Map[Int, Server] = Map()

  def init(max: Int): Unit = {
    servers = (1 to max).map(i => (i, new Server(i, max))).toMap
  }

  def apply(id: Int): Server = get(id)

  def get(id: Int): Server = servers(id)

  def debug(): Unit = {
    servers.toArray.sortBy(_._1).foreach(_._2.debug())
    println("-----------------------------------------")
  }

  def tick(): Unit = servers.toArray.sortBy(_._1).foreach(_._2.tick())
}

class Server(val id: Int, val max: Int) {
  val channels: Map[Int, Channel] = (1 to max).filter(_ != id).map(i => (i, new Channel(id, i))).toMap
  var valid: Boolean = true
  var state: State = Follower
  var currentTerm: Int = 0

  // follower
  var leaderHeartBeat: Int = (Math.random() * 6).toInt + 1

  // candidate
  var voteMap: Map[Int, (Int, Boolean)] = Map() // id -> (rest, res)

  def voteMapDebug: String = {
    voteMap.toArray.sortBy(_._1).map { case (no, (rest, res)) =>
      s"\tVoteMe: [no:${no}] (rest:${rest}, res:${res})"
    }.mkString("\n")
  }

  def debug(): Unit = {
    val info = s"[${id}] ${state} [term:${currentTerm}] " + (state match {
      case Follower => s"[heartbeat:${leaderHeartBeat}]"
      case Candidate => s"\n${voteMapDebug}"
      case Leader => ""
    })
    println(info)
    channels.foreach(_._2.debug())
  }

  def tick(): Unit = state match {
    case Follower =>
      leaderHeartBeat -= 1
      tryCandidate()
    case Candidate =>
  }

  def handle(msg: Msg): Unit = msg match {
    case Msg.VoteMe(leader, term) => term > currentTerm match {
      case false => channels(leader).send(Msg.VoteReply(id, false), 3) // 只接受term比自己大的情况，不然说明是历史的节点刚上线
      case true => channels(leader).send(Msg.VoteReply(id, true), 3)
    }
  }

  def tickVoteMap(): Unit = {
    voteMap.foreach { case (no, (rest, res)) =>
      (no, rest - 1, res)
    }
  }

  def tickChannel(): Unit = {
    channels.foreach(_._2.tick())
  }

  def tryCandidate(): Unit = {
    if (leaderHeartBeat == 0) {
      state = Candidate
      currentTerm += 1
      // 首先选择自己
      voteMap += (id -> (-1, true))
      (1 to max).filter(_ != id).foreach(i => voteMap += (i -> (7, false)))
      // 发送选举信息
      channels.foreach(_._2.send(Msg.VoteMe(id, currentTerm), 3))
    }
  }
}

class Channel(val from: Int, val to: Int) {
  var valid: Boolean = true
  var msgs: Array[(Int, Msg)] = Array() // rest, msg

  def debug(): Unit = {
    msgs.foreach(p => println(s"(${from})->(${to}): [rest:${p._1}, msg:<${p._2}>]"))
  }

  def tick(): Unit = {
    // 所有时间为0的进行发送
    msgs.filter(_._1 <= 0).foreach { case (_, m) => Servers(to).handle(m) }
    msgs = msgs.filter(_._1 > 0).map { case (i, m) => (i - 1, m) }
  }

  def send(msg: Msg, road: Int): Unit = {
    msgs ++= Array((road, msg))
  }

  //  override def receive = {
  //    case Msg.Debug =>
  //      msgs.foreach(p => println(s"${p._1}, ${p._2}"))
  //    case Msg.Tick =>
  //
  //    case m: Msg => msgs ++= Array((m.road, m))
  //    case _ => throw new UnreachableException
  //  }
}

class Msg

object Msg {

  case object Debug

  case object Tick

  case class VoteMe(id: Int, term: Int) extends Msg {
    override def toString = s"VoteMe leader:${id} term:${term}"
  }

  case class VoteReply(id: Int, res: Boolean) extends Msg

}

object Raft {
  def main(args: Array[String]): Unit = {
    //    val system: ActorSystem = ActorSystem()
    //    val actor: ActorRef = system.actorOf(Props(classOf[PongActor]))
    //    implicit val timeout: Timeout = akka.util.Timeout(1 second)
    Servers.init(3)
    while (true) {
      Servers.debug()
      Servers.tick()
    }
  }
}

