package module3.e20_catseffect_01





/*
import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.implicits._

object FilesAndHttpSolution extends IOApp.Simple {

  def program[F[_]: Console: FileSystem: HTTPClient: Monad]: F[Unit] = for {
    _ <- Console[F].printLine("Enter file path:")
    path <- Console[F].readLine
    data <- FileSystem[F].readFile(path)
    _ <- HTTPClient[F].postData("https://gosuslugi.ru", data)
  } yield ()

  def run: IO[Unit] = {
    import Interpreters._

    program[IO]
  }
}
 */
