import State.Follower
import akka.actor.CoordinatedShutdown.UnknownReason
import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}
import akka.util.Timeout

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

  def get(id: Int): ActorRef = servers(id)
}

class Server(val id: Int, val max: Int) extends Actor {
  val channels: Map[Int, ActorRef] = (1 to max).filter(_ != id).map(i => (i, Context.props(classOf[Channel], id, i))).toMap
  var valid: Boolean = true
  var state: State = Follower

  // follower
  var leaderHeartBeat: Int = (Math.random() * 6).toInt + 1

  // candidate
  var voteMeTimeout: Int = 0

  override def receive = { // follower
    case "test" => println("test")
    case Msg.Tick =>
      leaderHeartBeat -= 1
      if (leaderHeartBeat == 0) {
        becomeCandidate()
      }
    case msg => println(msg)
  }

  def becomeCandidate(): Unit = {
    context.become(candidate)
  }

  def candidate: Receive = {
    case Msg.Tick => tick()
  }

  def tick(): Unit = {

  }
}

class Channel(val from: Int, val to: Int) extends Actor {
  override def receive = {
    case Msg.Tick => tick()
    case _ => throw new UnreachableException
  }

  def tick(): Unit = {

  }
}

object Msg {

  case object Tick

  case class VoteMe(id: Int)

}

object Raft {
  def main(args: Array[String]): Unit = {
    //    val system: ActorSystem = ActorSystem()
    //    val actor: ActorRef = system.actorOf(Props(classOf[PongActor]))
    //    implicit val timeout: Timeout = akka.util.Timeout(1 second)
    Servers.init(3)
  }
}

