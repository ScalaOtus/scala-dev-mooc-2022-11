import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.concurrent.Future

implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

val pure = IO.pure("pure value")

val sideEffect = IO.delay(println("hello!"))
val mistake = IO.pure(println("hello mistake!"))

val fromEither: IO[Int] = IO.fromEither(Left(new Exception("Noooooo!")))
val fromFuture =
  IO.fromFuture(IO.delay(Future.successful("Back to the future")))

val failing: IO[Int] = IO.raiseError(new Exception("aaa"))

IO.never[Int]

val async = IO.async_((cb: Either[Throwable, Int] => Unit) =>
  Future(Thread.sleep(500))
    .map(_ => 100)
    .onComplete(aTry => cb(aTry.toEither))
)

// combinators
async.map(_ + 100).unsafeRunSync()

async
  .flatMap(number => fromFuture.map(string => string + number.toString))
  .unsafeRunSync()

(sideEffect *> fromFuture).unsafeRunSync()

val intIO: IO[Int] = async

def wantInt(i: Int) = i * 100
