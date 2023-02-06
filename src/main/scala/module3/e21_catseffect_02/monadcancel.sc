import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global
import cats.implicits._

import scala.concurrent.duration._
implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

// forceR
val a = IO.pure(42).productR(IO.pure(10))
val willFail = IO.raiseError(new RuntimeException("Boom!")) *> IO.pure(10)
willFail.unsafeRunSync()

val wontFail = IO.raiseError(new RuntimeException("Boom!")).forceR(IO.pure(42))
wontFail.unsafeRunSync()

// uncancelable
val justSleep =
  (IO.sleep(1.second) *> IO.println("Still not cancelled")).uncancelable
val alsoSleepAndThrow =
  IO.sleep(100.millis) *> IO.raiseError(new RuntimeException("Boom!"))

(justSleep, alsoSleepAndThrow).parTupled.unsafeRunSync()

// onCancel

val justSleep =
  (IO.sleep(1.second) *> IO.println("Still not cancelled"))
    .onCancel(IO.println("I'm getting cancelled"))
val alsoSleepAndThrow =
  IO.sleep(100.millis) *> IO.raiseError(new RuntimeException("Boom!"))

(justSleep, alsoSleepAndThrow).parTupled.unsafeRunSync()
