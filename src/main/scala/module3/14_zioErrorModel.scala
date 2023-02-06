package module3

import zio.{IO, Task, UIO, URIO}
import zio.console.{Console, putStrLn}

object zioErrorHandling {

  sealed trait Cause[+E]

  object Cause {

    final case class Fail[E](e: E) extends Cause[E]

    final case class Die(t: Throwable) extends Cause[Nothing]

  }


  case class ZIO[-R, +E, +A](run: R => Either[E, A]) {self =>

    /**
      * 
      * Базовый оператор для работы с ошибками
      */

    def foldM[R1 <: R, E1, B](
             failure: E => ZIO[R1, E1, B],
             success: A => ZIO[R1, E1, B]
             ): ZIO[R1, E1, B] =
      ZIO(r => self.run(r).fold(
        e => failure(e),
        a => success(a)
      ).run(r))


    def orElse[R1 <: R, E1, A1 >: A](other: ZIO[R1, E1, A1]): ZIO[R1, E1, A1] =
      foldM(
        _ => other,
        v => ZIO(r => Right(v))
      )

    /**
     * Реализовать метод, котрый будет игнорировать ошибку в случае падения,
     * а в качестве результата возвращать Option
     */
    def option: ZIO[R, Nothing, Option[A]] =
      foldM(
        _ => ZIO(r => Right(None)),
        v => ZIO(r => Right(Some(v)))
      )

    /**
     * Реализовать метод, котрый будет работать с каналом ошибки
     */
    def mapError[E1](f: E => E1): ZIO[R, E1, A] =
      foldM(
        e => ZIO(_ => Left(f(e))),
        v => ZIO(_ => Right(v))
      )


  }




  sealed trait IntegrationError

  case object ConnectionFailed extends IntegrationError

  case object CommunicationFailed extends IntegrationError

  lazy val connect: IO[ConnectionFailed.type, Unit] = ???

  lazy val getSomeData: IO[CommunicationFailed.type, String] = ???

  lazy val connectAndGetData: IO[IntegrationError, String] = connect *> getSomeData


  lazy val io1: IO[String, String] = ???

  lazy val io2: IO[Int, String] = ???

  /**
   * 1. Какой будет тип на выходе, если мы скомбинируем эти два эффекта с помощью zip
   */

   val z1: zio.IO[Any,(String, String)] = io1 zip io2

  /**
   * Можем ли мы как-то избежать потерю информации об ошибке, в случае композиции?
    */

  lazy val io3: zio.IO[Either[String, Int], (String, String)] =
    io1.mapError(Left(_)).zip(io2.mapError(Right(_)))



  def either: Either[String, Int] = ???

  def errorToErrorCode(str: String): Int = ???

  lazy val effFromEither: IO[String, Int] = zio.ZIO.fromEither(either)

  /**
   * Залогировать ошибку effFromEither, не меняя ее тип и тип возвращаемого значения
   */
  lazy val z2: zio.IO[String, Int] = effFromEither.tapError{ e =>
    zio.ZIO.effect(println(e)).orDie
  }


  /**
   * Изменить ошибку effFromEither
   */

  lazy val z3: zio.IO[Int, Int] = effFromEither.mapError(errorToErrorCode)


  lazy val z4 = ???


  // трансформировать ошибку
  lazy val z5 = ???


  // Разные типы ошибок

    type User = String
    type UserId = Int

    sealed trait NotificationError
    case object NotificationByEmailFailed extends NotificationError
    case object NotificationBySMSFailed extends NotificationError

    def getUserById(userId: UserId): Task[User] = ???

    def sendEmail(user: User, msg: String): IO[NotificationByEmailFailed.type, Unit] = ???

    def sendSMS(user: User, msg: String): IO[NotificationBySMSFailed.type, Unit] = ???

    def sendNotification(userId: UserId): IO[NotificationError, Unit] = for{
      user <- getUserById(1).orDie
      _ <- sendEmail(user, "email")
      _ <- sendSMS(user, "9999")
    } yield ()

}