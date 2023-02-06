package module3.e22_catseffect_03

import cats.implicits._

import cats.effect.{Resource, IOApp, IO, Ref, Deferred}

object Main extends IOApp.Simple {

  /** Process a command
    * @return true if program has to exit
    */
  def process(env: Environment)(cmd: Command): IO[Boolean] =
    cmd match {
      case Command.Echo =>
        IO.readLine.flatMap(IO.println).as(true)
      case Command.Exit =>
        IO.pure(false)
      case Command.ReadNumber =>
        env.mutable.get.flatMap(IO.println).as(true)
      case Command.AddNumber(num) =>
        env.mutable.update(i => i + num).as(true)
      case Command.ReleaseTheDogs =>
        env.promise.complete(()).as(true)
      case Command.LaunchDog(name) =>
        val fiber = IO.println(s"Dog $name ready to start!") *>
          env.promise.get *>
          IO.println(s"Dog $name starting!") *>
          env.mutable
            .updateAndGet(_ + 1)
            .flatMap(number => IO.println(s"Dog $name observed number $number"))

        fiber.start.as(true)

    }

  def program(env: Environment): IO[Unit] =
    IO.readLine.flatMap { cmd =>
      Command.parse(cmd) match {
        case Right(command) =>
          process(env)(command).flatMap {
            case true  => IO.print("> ") *> program(env)
            case false => IO.println("Bye bye")
          }
        case Left(error) => IO.println(error)
      }
    }

  final case class Environment(
      mutable: Ref[IO, Int],
      promise: Deferred[IO, Unit]
  )

  object Environment {
    def build: Resource[IO, Environment] = {
      val ref = Resource.make(Ref.of[IO, Int](0))(ref =>
        ref.get.flatMap { i => IO.println(s"Destroying ref with $i") }
      )
      val promise = Resource.make(Deferred[IO, Unit])(_ =>
        IO.println(s"Destroying promise")
      )

      for {
        r <- ref
        p <- promise
      } yield Environment(r, p)
    }
  }

  def run: IO[Unit] =
    Environment.build.use { env =>
      IO.print("> ") *> program(env)
    }
}
