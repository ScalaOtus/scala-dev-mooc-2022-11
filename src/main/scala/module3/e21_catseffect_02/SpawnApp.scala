package module3.e21_catseffect_02

import cats.effect.{IO, IOApp, Spawn}

import scala.concurrent.duration._

object SpawnApp extends IOApp.Simple {
  def longRunningIO: IO[Unit] =
    (IO.sleep(100.millis) *>
      IO.println(s" Hello from ${Thread.currentThread()}"))
      .flatMap(_ => longRunningIO)

  def run: IO[Unit] =
    (Spawn[IO].start(longRunningIO) *>
      IO.println("The fiber is running in parallel")) *>
      IO.sleep(2.seconds)
}
