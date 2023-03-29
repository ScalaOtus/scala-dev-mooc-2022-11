package Akka
import Akka.intro_actors.behaviour_factory_methods
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, SpawnProtocol}
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.Props

import scala.concurrent.Future
import scala.language.{existentials, postfixOps}
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}


//1.
object AkkaMain {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem[String](behaviour_factory_methods.Echo(), "Echo")
    system ! "Hello"
    Thread.sleep(3000)
   // system.terminate()
  }

}

//2. root actor

object AkkaMain2{
  object Supervisor {
    def apply(): Behavior[SpawnProtocol.Command] = Behaviors.setup{ctx=>
      ctx.log.info(ctx.self.toString)
      SpawnProtocol()
    }
  }


  def main(args: Array[String]): Unit = {

    implicit  val system = ActorSystem[SpawnProtocol.Command](Supervisor(), "Echo")
    implicit val ec = system.executionContext
    implicit val timeout = Timeout(3 seconds)

    val echo: Future[ActorRef[String]] = system.ask(
      SpawnProtocol.Spawn(intro_actors.behaviour_factory_methods.Echo(),
      "Echo", Props.empty, _))

    for (ref <- echo)
      ref ! "Hello from ask"
  }
}

//3. chnage state
object AkkaMain3{
  object chnage_behaviour{
    sealed trait WorkerProtocol
    object WorkerProtocol{
      case object Start extends WorkerProtocol
      case object StandBy extends WorkerProtocol
      case object Stop extends  WorkerProtocol
    }

    import  WorkerProtocol._
    def apply(): Behavior[WorkerProtocol] = idle()
    def idle():Behavior[WorkerProtocol] = Behaviors.setup{ ctx =>
      Behaviors.receiveMessage{
        case msg@Start =>
          ctx.log.info(msg.toString())
          workInProgress()
        case msg@StandBy =>
          ctx.log.info(msg.toString())
          idle()
        case msg@Stop =>
          ctx.log.info(msg.toString())
          Behaviors.stopped
      }
    }


    def workInProgress():Behavior[WorkerProtocol] = Behaviors.setup{ ctx =>
      Behaviors.receiveMessage{
        case msg@Start => Behaviors.unhandled
        case msg@StandBy =>
          ctx.log.info("go to StandBy")
          idle()
        case msg@Stop =>
          ctx.log.info("stopped")
          Behaviors.stopped
      }
    }
  }
}


//4. state with calculations
object handle_state{
  object Counter{
    sealed trait CounterProtocol
    object CounterProtocol{
      final case object Inc extends  CounterProtocol
      final case class GetCounter(replyTo: ActorRef[Int]) extends CounterProtocol
    }

    import CounterProtocol._
    def apply(init: Int): Behavior[CounterProtocol] = inc(init)
    def inc(counter: Int): Behavior[CounterProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case Inc =>
          inc(counter + 1)
        case GetCounter(replyTo) =>
          replyTo ! counter
          Behaviors.same
      }
    }
  }
}


//5.
object task_dispatcher{
  sealed trait  TaskDispatcherProtocol
  case class ParseUrl(url: String) extends TaskDispatcherProtocol
  case class Log(str: String) extends TaskDispatcherProtocol

  case class LogResponseWrapper(msg: LogWorker.ResponseProtocol) extends  TaskDispatcherProtocol
  case class ParseResponseWrapper(msg: ParseUrlWorker.ResponseProtocol) extends  TaskDispatcherProtocol


  def apply(): Behavior[TaskDispatcherProtocol] = Behaviors.setup{ctx =>
    val adapter1 = ctx.messageAdapter[LogWorker.ResponseProtocol](rs => LogResponseWrapper(rs))
    val adapter2 = ctx.messageAdapter[ParseUrlWorker.ResponseProtocol](rs => ParseResponseWrapper(rs))

    Behaviors.receiveMessage{
      case ParseUrl(url) =>
        val ref = ctx.spawn(ParseUrlWorker(), s"ParseWorker - ${java.util.UUID.randomUUID().toString}")
        ref ! ParseUrlWorker.ParseUrl(url, adapter2)
        Behaviors.same
      case Log(str) =>
        val ref = ctx.spawn(LogWorker(), "djbmdjbgfdkjfbg")
        ref ! LogWorker.Log(str, adapter1)
        Behaviors.same
      case LogResponseWrapper(LogWorker.LogDone) =>
        //logging etwa.
        Behaviors.same
      case ParseResponseWrapper(ParseUrlWorker.ParseDone) =>
        // machen etwa
        Behaviors.same
    }


  }


  object LogWorker{
    sealed trait  LogProtocol
    case class Log(str: String, replyTo: ActorRef[ResponseProtocol]) extends  LogProtocol

    sealed trait  ResponseProtocol
    case object  LogDone extends  ResponseProtocol

    def apply(): Behavior[LogProtocol]= Behaviors.setup{ctx=>
      Behaviors.receiveMessage{
        case Log(str, replyTo) =>
          //we are making something
          replyTo ! LogDone
          // end
          Behaviors.stopped
      }
    }
  }

  object ParseUrlWorker{
    sealed trait ParseProtocol
    case class ParseUrl(url: String, replyTo: ActorRef[ResponseProtocol]) extends  ParseProtocol

    sealed  trait  ResponseProtocol

    case object  ParseDone extends ResponseProtocol

    def apply(): Behavior[ParseProtocol] = Behaviors.setup{ctx=>
      Behaviors.receiveMessage{
        case ParseUrl(str, replyTo) =>
          //we are making something
          replyTo ! ParseDone
          // end
          Behaviors.stopped
      }
    }
  }

}


