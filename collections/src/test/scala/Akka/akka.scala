package Akka

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.ActorRef
import akka.actor.typed.SpawnProtocol
import akka.actor.typed.{PostStop}

object intro_actors{
  // 1. functional
  object behaviour_factory_methods{
    object Echo{
      def apply(): Behavior[String] = Behaviors.setup{ ctx =>
        Behaviors.receiveMessage{
          case msg =>
            ctx.log.info(msg)
            Behaviors.same
        }
      }
    }
  }

  //2. OOP
  object abstract_behaviour {
    object Echo{
      def apply(): Behavior[String] = Behaviors.setup{ ctx =>
        new Echo(ctx)
      }
    }

    class Echo(ctx: ActorContext[String]) extends AbstractBehavior[String](ctx){
      def onMessage(msg: String): Behavior[String] ={
        ctx.log.info(msg)
        this
      }
    }

  }

}