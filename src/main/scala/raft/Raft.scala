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
  private var servers: Map[Int, ActorRef] = Map()

  def init(max: Int): Unit = {
    servers = (1 to max).map(i => (i, Context.props(classOf[Server], i, max))).toMap
  }

  def apply(id: Int): ActorRef = get(id)

  def get(id: Int): ActorRef = servers(id)

  def debug(): Unit = servers.toArray.sortBy(_._1).foreach(_._2 ! Msg.Debug)

  def tick(): Unit = servers.toArray.sortBy(_._1).foreach(_._2 ! Msg.Tick)
}

class Server(val id: Int, val max: Int) extends Actor {
  val channels: Map[Int, ActorRef] = (1 to max).filter(_ != id).map(i => (i, Context.props(classOf[Channel], id, i))).toMap
  var valid: Boolean = true
  var state: State = Follower
  var currentTerm: Int = 0

  // follower
  var leaderHeartBeat: Int = (Math.random() * 6).toInt + 1

  // candidate
  var voteMeTimeout: Int = 0
  var voteMap: Map[Int, (Int, Boolean)] = Map() // id -> (rest, res)

  override def receive = {
    case Msg.Debug =>
      val info = s"[${id}] ${state} [term:${currentTerm}] " + (state match {
        case Follower => s"[hb:${leaderHeartBeat}]"
        case Candidate => s"[vt:${voteMeTimeout}] [vs:${voteMap}]"
        case Leader => ""
      })
      println(info)
      channels.foreach(_._2 ! Msg.Debug)
    case Msg.Tick => state match {
      case Follower =>
        leaderHeartBeat -= 1
        tryCandidate()
      case Candidate =>

    }
    case msg => println(msg)
  }

  def tickChannel(): Unit = {
    channels.foreach(p => p._2 ! Msg.Tick)
  }

  def tryCandidate(): Unit = {
    if (leaderHeartBeat == 0) {
      state = Candidate
      currentTerm += 1
      // 首先选择自己
      voteMap += (id -> (-1, true))
      // 发送选举信息
      channels.foreach(_._2 ! Msg.VoteMe(id, 3, currentTerm))
    }
  }
}

class Channel(val from: Int, val to: Int) extends Actor {
  var valid: Boolean = true
  var msgs: Array[(Int, Msg)] = Array() // rest, msg

  override def receive = {
    case Msg.Debug =>
      msgs.foreach(p => println(s"${p._1}, ${p._2}"))
    case Msg.Tick =>
      // 所有时间为0的进行发送
      msgs.filter(_._1 <= 0).foreach { case (_, m) => Servers(to) ! m }
      msgs = msgs.filter(_._1 > 0).map { case (i, m) => (i - 1, m) }
    case m: Msg => msgs ++= Array((m.road, m))
    case _ => throw new UnreachableException
  }
}

class Msg(val road: Int)

object Msg {

  case object Debug

  case object Tick

  case class VoteMe(id: Int, term: Int, r: Int) extends Msg(r)

}

object Raft {
  def main(args: Array[String]): Unit = {
    //    val system: ActorSystem = ActorSystem()
    //    val actor: ActorRef = system.actorOf(Props(classOf[PongActor]))
    //    implicit val timeout: Timeout = akka.util.Timeout(1 second)
    Servers.init(3)
    Servers.debug()
    Servers.tick()
    Servers.debug()
  }
}

