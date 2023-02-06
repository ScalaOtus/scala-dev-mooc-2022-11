package module3

import zio.{Ref, UIO, URIO, ZIO, clock}
import zio.clock.{Clock, sleep}
import zio.console.{Console, putStrLn}
import zio.duration.durationInt
import zio.internal.Executor

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.language.postfixOps


object zioConcurrency {


  // эфект содержит в себе текущее время
  val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)




  /**
   * Напишите эффект, который будет считать время выполнения любого эффекта
   */


   // 1. время начала
   // 2. выполнить код
   // 3. время конца
   // 4.

  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]) = for{
     start <- currentTime
     r <- zio
     end <- currentTime
     _ <- putStrLn(s"Running time ${end - start}")
   } yield r


  val exchangeRates: Map[String, Double] = Map(
    "usd" -> 76.02,
    "eur" -> 91.27
  )

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  val sleep1Second: URIO[Clock, Unit] = ZIO.sleep(1 seconds)

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  val sleep3Seconds = ZIO.sleep(3 seconds)

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation1 спустя 3 секунды
   */
     lazy val getExchangeRatesLocation1 =
       sleep3Seconds *> putStrLn("GetExchangeRatesLocation1")

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation2 спустя 1 секунду
   */
      lazy val getExchangeRatesLocation2 =
        sleep1Second *> putStrLn("GetExchangeRatesLocation2")



  /**
   * Написать эффект котрый получит курсы из обеих локаций
   */
  lazy val getFrom2Locations =
    getExchangeRatesLocation1 <*> getExchangeRatesLocation2

  /**
   * Написать эффект котрый получит курсы из обеих локаций паралельно
   */
  lazy val getFrom2LocationsInParallel = for{
    fiber <- getExchangeRatesLocation1.fork
    r2 <- getExchangeRatesLocation2
    r1 <- fiber.join
  } yield (r1, r2)


  /**
   * Предположим нам не нужны результаты, мы сохраняем в базу и отправляем почту
   */


   lazy val writeUserToDB: ZIO[Console with Clock, Nothing, Unit] = sleep1Second *> putStrLn("writeUserToDB").orDie

   lazy val sendMail: ZIO[Console with Clock, Nothing, Unit] = sleep1Second *> putStrLn("sendMail").orDie

  /**
   * Написать эффект котрый сохранит в базу и отправит почту паралельно
   */


  lazy val writeAndSand = for{
    _ <- writeUserToDB.fork
    _ <- sendMail.fork
    _ <- ZIO.sleep(1 seconds)
  } yield ()


  /**
   *  Greeter
   */

  lazy val greeter = (sleep1Second *> putStrLn("Hello")).forever

  val g1 = for{
    fiber <- ZIO.effect(while(true) println("Hello")).fork
    _ <- ZIO.sleep(5 seconds)
    _ <- fiber.interrupt
    _<- ZIO.sleep(2 seconds)
  } yield()


  /***
   * Greeter 2
   * 
   * 
   * 
   */

 lazy val hello: ZIO[Console, IOException, Nothing] = (putStrLn("Hello") *> hello)

 lazy val greeter2 = ???
  

  /**
   * Прерывание эффекта
   */

   lazy val app3 = ???





  /**
   * Получние информации от сервиса занимает 1 секунду
   */
  def getFromService(ref: Ref[Int]) = ???

  /**
   * Отправка в БД занимает в общем 5 секунд
   */
  def sendToDB(ref: Ref[Int]): ZIO[Clock with Console, Exception, Unit] = ???


  /**
   * Написать программу, которая конкурентно вызывает выше описанные сервисы
   * и при этом обеспечивает сквозную нумерацию вызовов
   */

  
  lazy val app1 = ???

  /**
   *  Concurrent operators
   */




  lazy val p1 = getExchangeRatesLocation1 zipPar getExchangeRatesLocation2

  lazy val p2 = getExchangeRatesLocation1 race getExchangeRatesLocation2

  lazy val p3 = ZIO.foreachPar(List(1, 2, 3, 4, 5))(el =>
    (sleep1Second *> putStrLn(el.toString))
  )






  /**
   * Lock
   */


  // Правило 1
  lazy val doSomething: UIO[Unit] = ???
  lazy val doSomethingElse: UIO[Unit] = ???

  lazy val executor: Executor = ???

  lazy val eff = for{
    f1 <- doSomething.fork
    _ <- doSomethingElse
    r <- f1.join
  } yield r

  lazy val result = eff.lock(executor)



  // Правило 2
  lazy val executor1: Executor = ???
  lazy val executor2: Executor = ???



  lazy val eff2 = for{
      f1 <- doSomething.lock(executor2).fork
      _ <- doSomethingElse
      r <- f1.join
    } yield r

  lazy val result2 = eff2.lock(executor1)  



}