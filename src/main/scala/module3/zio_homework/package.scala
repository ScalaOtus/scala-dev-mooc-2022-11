package module3

import zio.{Has, RIO, Task, UIO, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps
import module3.zio_homework.config._
import module3.zioConcurrency.{currentTime, printEffectRunningTime}

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val readLine: ZIO[Console, IOException, String] = getStrLn
  lazy val readInt: RIO[Console, Int] = readLine.flatMap(str => ZIO.effect(str.toInt))

  lazy val guessProgram: RIO[Console with Random, Unit] = for {
    _ <- putStrLn("Введите число от 1 до 3: ")
    userInt <- readInt
    randomInt <- nextIntBetween(1, 3)
    _ <- putStrLn(if (randomInt == userInt) "Успех угадывания числа!" else "Ошибка угадывания числа!")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A], f: A => Boolean):ZIO[R, E, A] =
    effect.flatMap(obj => if (f(obj)) ZIO.succeed(obj) else doWhile(effect, f))


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: URIO[Console, AppConfig] =
    for {
      conf <- config.load.orElse(Task.succeed(AppConfig("localhost", "8888")))
      _ <- putStrLn(conf.toString)
    } yield conf


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random with Clock, Int] = ZIO.sleep(1 second) *> nextIntBetween(0, 10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = for (_ <- 0 to 10) yield eff

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: URIO[Clock with Random, Int] =
    printEffectRunningTime {
      for {
        effect <- ZIO.collectAll(effects).map(_.sum)
      } yield effect
  }


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: URIO[Clock with Random, Int] =
    printEffectRunningTime {
      for {
        effect <- ZIO.collectAllPar(effects).map(_.sum)
      } yield effect
  }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type EffectPrinter = Has[EffectPrinter.Service]

  object EffectPrinter {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    class ServiceImpl extends Service {
      override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
        for {
          start <- currentTime
          r <- zio
          end <- currentTime
          _ <- ZIO.effect(println(s"Running time ${end - start}")).orDie
        } yield r

    }

    val live: ULayer[EffectPrinter] = ZLayer.succeed(new ServiceImpl)

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[EffectPrinter with Console with Clock with R, E, A] =
      ZIO.accessM(_.get.printEffectRunningTime(zio))
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: URIO[EffectPrinter with Console with Clock with Random, Int] =
    EffectPrinter.printEffectRunningTime(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Console with Random with Clock](EffectPrinter.live)

}
