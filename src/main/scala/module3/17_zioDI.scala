package module3

import zio.{Has, IO, RIO, Task, URIO, ZIO, ZLayer}
import zio.clock.Clock
import zio.console.Console
import zio.duration.durationInt
import zio.random.Random

import scala.language.postfixOps
import module1.type_system

object di {

  type Query[_]
  type DBError
  type QueryResult[_]
  type Email
  type User


  trait DBService{
    def tx[T](query: Query[T]): IO[DBError, QueryResult[T]]
  }

  trait EmailService{
    def makeEmail(email: String, body: String): Task[Email]
    def sendEmail(email: Email): Task[Unit]
  }

  trait LoggingService{
    def log(str: String): Task[Unit]
  }

  trait UserService{
      def getUserBy(id: Int): RIO[LoggingService, User]
  }



  type MyEnv = Random with Clock with Console

  /**
   * Написать эффект который напечатет в консоль приветствие, подождет 5 секунд,
   * сгенерит рандомное число, напечатает его в консоль
   *   Console
   *   Clock
   *   Random
   */


  def e1: URIO[Random with Clock with Console, Unit] = for{
    console <- ZIO.environment[Console].map(_.get)
    clock <-   ZIO.environment[Clock].map(_.get)
    random <-  ZIO.environment[Random].map(_.get)
    _ <- console.putStrLn("Hello").orDie
    _ <- clock.sleep(5 seconds)
    int <- random.nextInt
    _ <- console.putStrLn(int.toString).orDie
  } yield()


  def e2: URIO[MyEnv, Unit] = e1

  lazy val getUser:
    ZIO[LoggingService with UserService, Throwable, User] = ZIO.environment[UserService].flatMap(_.getUserBy(10))

  lazy val sendMail: ZIO[EmailService, Throwable, Unit] = ???


  /**
   * Эффект, который будет комбинацией двух эффектов выше
   */
  lazy val combined2:
    ZIO[EmailService with LoggingService with UserService, Throwable, (User, Unit)] = getUser <*> sendMail


  /**
   * Написать ZIO программу которая выполнит запрос и отправит email
   */
  lazy val queryAndNotify: ZIO[LoggingService with EmailService with UserService, Throwable, Unit] = for{
    userService <- ZIO.environment[UserService]
    emailService <- ZIO.environment[EmailService]
    user <- userService.getUserBy(10)
    email <- emailService.makeEmail("", "")
    _ <- emailService.sendEmail(email)
  } yield ()



  lazy val services: UserService with EmailService with LoggingService = ???

  lazy val dBService: DBService = ???
  lazy val userService: UserService = ???

  lazy val emailService2: EmailService = ???

  def f(userService: UserService): UserService with EmailService with LoggingService = ???

  // provide
  lazy val e3: IO[Throwable, Unit] = queryAndNotify.provide(services)

  // provide some
  lazy val e4: ZIO[UserService, Throwable, Unit] = queryAndNotify.provideSome[UserService](f)
  
  // provide
  lazy val e5: IO[Throwable, Unit] = e4.provide(userService)
}