package module3.e20_catseffect_01

import cats.effect.{IO, IOApp}

object FilesAndHttp extends IOApp.Simple {

  def program[F[_]]: F[Unit] = ???

  def run: IO[Unit] = {

    program[IO]
  }
}
