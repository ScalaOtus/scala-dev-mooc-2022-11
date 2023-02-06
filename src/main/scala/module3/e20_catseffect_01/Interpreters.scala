package module3.e20_catseffect_01

import cats.effect.IO

object Interpreters {

  // todo: console

  implicit val fileSystemIO: FileSystem[IO] = new FileSystem[IO] {
    def readFile(path: Path): IO[Path] =
      IO.pure(s"this is file with all the passwords at $path")
  }

  implicit val httpClientIO: HTTPClient[IO] = new HTTPClient[IO] {
    def postData(url: URI, body: Path): IO[Unit] =
      IO.delay(println(s"POST '$url': $body"))
  }
}
