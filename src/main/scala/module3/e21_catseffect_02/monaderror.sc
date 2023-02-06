import cats.MonadError
import cats.data.State
import cats.effect.IO
import cats.implicits._

type MyMonad[F[_]] = MonadError[F, Unit]
type MyError[A] = Either[Unit, A]

val stateFor = for {
  a <- State { (s: Int) => (s + 1, s) }
  b <- State { (s: Int) => (s + 1, s) }
  c <- State { (s: Int) => (s + 1, s) }
  d <- State { (s: Int) => (s + 1, s) }
} yield a + b + c + d

val optionFor = for {
  a <- Some(3)
  b <- Some(3)
  c <- None
  d <- Some(3)
} yield a + b + c + d

// Option

def withError[F[_]: MyMonad]: F[Int] =
  for {
    a <- MonadError[F, Unit].pure(42)
    b <- MonadError[F, Unit].pure(42)
    _ <- MonadError[F, Unit].raiseError[Int](())
    c <- MonadError[F, Unit].pure(42)
  } yield a + b + c

val a: Option[Int] = withError[Option]

def withHandling[F[_]: MyMonad]: F[String] = {
  val value: F[String] = MonadError[F, Unit].raiseError(())
  value.handleError(_ => "There was an error")
}

def withAttempt[F[_]: MyMonad]: F[Either[Unit, String]] = {
  val value: F[String] = MonadError[F, Unit].raiseError(())
  value.attempt
}

withHandling[Option]
withAttempt[Option]

val ioError = IO.raiseError(new RuntimeException("Boom!"))
ioError.attempt *> IO.println("Hello, World!")
