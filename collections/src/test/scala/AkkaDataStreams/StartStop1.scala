import AkkaDataStreams.{Parent, SendMessageToChild, StartChild, StopChild}
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.DeathPactException
import akka.actor.typed.SupervisorStrategy
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpecLike


object Protocol{
  sealed trait Command
  case class Fail(text: String) extends  Command
  case class Hello(text: String, replyTo: ActorRef[String]) extends Command
}

import Protocol._

object Worker {
  def apply(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Fail(text) =>
        throw new RuntimeException(text)
      case Hello(text, replyTo) =>
        replyTo ! text
        Behaviors.same
    }
}

object MiddleManagment{
  def apply(): Behavior[Command] =
    Behaviors.setup[Command]{ context =>
      context.log.info("MiddleManagment starting up")
      val child = context.spawn(Worker(), "child")
      context.watch(child)

      Behaviors.receiveMessage { message =>
        child ! message
        Behaviors.same
      }
    }
}

object Boss{
  def apply(): Behavior[Command] =
    Behaviors.supervise(

      Behaviors.setup[Command]{context =>

        context.log.info("Boss starting up")
        val middleManagment = context.spawn(MiddleManagment(), "middle-managment")
        context.watch(middleManagment)

        Behaviors.receiveMessage[Command]{message =>
          middleManagment ! message
          Behaviors.same


        }

      }


    ).onFailure[DeathPactException](SupervisorStrategy.restart)
}

class StartStopSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike{
  "Typed probe actor" must {
    "send back message" in {
      val boss = spawn(Boss(), "ipepr-management")
      val replyProbe = createTestProbe[String]()
      boss ! Hello("hi 1", replyProbe.ref)
      replyProbe.expectMessage("hi 1")

      boss.tell(Fail("ping"))

      eventually{
        boss ! Hello("hi 2", replyProbe.ref)
        replyProbe.expectMessage(200.millis, "hi 2")
      }

    }


  }


}