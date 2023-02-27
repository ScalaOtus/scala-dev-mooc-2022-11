package catseffect
import cats.effect.std.{Console, Semaphore, Supervisor}
import cats.effect.unsafe.implicits.global
import cats.effect.*
import cats.implicits.*

import java.net.URI
import scala.concurrent.Future
import scala.concurrent.duration.*
import cats.Monad


object  catseffects {

  @main def Ex(): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

    //Очень важно, все что сейсчас будем писать это рецепт, это не выполнение
    //конструкторы, как в ZIO в принципе
    val pure = IO.pure("pure value") // создание без сайд эффектов
    val sideeffect = IO.delay(println("dg")) // здесь передаем по значению (thunk: => A), так как эта штука должна выполниться когда IO запустится,
    // тоесть println сейчас выполнит ся не должен
    val mistake = IO.pure(println("error"))
    //есть метод, который нельзя использовать в реальных проектах. Эьа функция просто выполняет IO, в реальном приложении
    // выполнение где то на самом верхнем уровне, а еще оно sync
    sideeffect.unsafeRunSync()
    sideeffect.unsafeRunSync()
    mistake.unsafeRunSync()
    mistake.unsafeRunSync()
    // вопрос, что здесь будет?
    // создается side эффект и печатается println и mistake ничего не напечатает, так как в IO лежит просто unit

    //-------------------------------------
    //есть много разных конструкторов
    //можем создать из Either

    // в IO throwable внутри должен быть, потому в left надо что то
    // c ошибкой
    val fromEither = IO.fromEither(Left(new Exception("fail")))
    // можем из future создать, тут интересно она ждет не фьючу, а IO[Future]
    // почему? потому что если просто положить фьючу, то он будет запущен,
    // он не будет ссылочно прозрачен, он каждый раз не будет выполняться
    val fromFuture = IO.fromFuture(IO.delay(Future.successful(1)))

    //для IO с ошибкой
    // в отличие от ZIO тут может быть только Throwable
    //вопрос,какой будет тип?
    //тип Nothing, потому что неизветсно какой он будет
    //это не хорошо, лучше типы проставлять самим
    val failing: IO[Nothing] = IO.raiseError(new Exception("error"))


    // интересный комбинатор
    // это IO который никогда не закончится
    // это бусконечный цикл, который не ест ресурсы
    //используется в кишках для запуска серверов
    //вопрос,какой будет тип?
    //тип Nothing, потому что неизветсно какой он будет в соучае успеха, а потому тут будет Nothing
    val never: IO[Nothing] = IO.never
    //добавляем типы
    val failingtyped: IO[Int] = IO.raiseError(new Exception("error"))


    //последний конструктор, самый сложный async
    //посмотреть на сигнатуру
    //def async_[A](k: (Either[Throwable, A] => Unit) => Unit): IO[A] = {
    // это способ подружить либы с IO с либами с callback
    // асинк позволяет вклинится в IO runtime с нефункциональным кодом
    // (Either[Throwable, A] => Unit) - это сигнатура callback
    // IO.async_()


    //наше не функциональное вычисление
    val future = Future(Thread.sleep(2000)).map(_ => 100)

    val async: IO[Int] = IO.async_(
      //тут мы вклиниваемся с нефункциональным кодом
      //IO runtime выдает некоторый callback который надо вызвать чтобы IO получил результат
      (cb: Either[Throwable, Int] => Unit) =>
        future.onComplete(a => cb(a.toEither))
    )

    //тоесть мы передаем некоторое вычисление, которое заворачивается в IO и может быть как то далее использовано

    //я тебе дам коллбэк и ты его вызови по окончанию вычисления и получишь результат
    async.unsafeRunSync()


    //пара комбинаторов
    //map преобразование без сайдэффектов
    println(async.map(_ + 200).unsafeRunSync())
    //добавим сайд эффект println
    async.flatMap(i => IO.println(i)).unsafeRunSync()


    //best practic
    //    IO {
    //      readingFile
    //      writingToDatabase
    //      sendBytesOverTcp
    //      launchMissiles
    //    }
    /*val program =
for {
  data <- readFile
  _    <- writeToDatabase(data)
  _    <- sendBytesOverTcp(data)
  _    <- launchMissiles
} yield ()*/

    //Use pure functions in map / flatMap
    IO.pure(123).map(n => println(s"NOT RECOMMENDED! $n"))
    //This too should be avoided, because the side effect is not suspended in the returned IO value:
    IO.pure(123).flatMap { n =>
      println(s"NOT RECOMMENDED! $n")
      IO.unit
    }

    //The correct approach would be this:
    IO.pure(123).flatMap { n =>
      // Properly suspending the side effect
      IO(println(s"RECOMMENDED! $n"))
    }
  }

  //сколько нибудь реальное приложение в Simple где то дергается реальный вызов sync или async
  object DataConnectionHttpIO extends IOApp.Simple {
    def readHive(path: String): IO[String] =
      IO.pure(s"here we are reading from hive")

    def readFileCsv(path: String): IO[String] =
      IO.pure(s"here we are reading from file")

    def run: IO[Unit] = {
      for {
        _ <- IO.delay(println("get setting from config"))
        path <- IO.readLine
        data <- readHive(path)
      } yield ()
    }
  }


  //--------------------------------------
  // tagless final
  //--------------------------------------

  //здесь IO фиксировано
  object FilesAndHttpIO extends IOApp.Simple {
    def readFile(file: String): IO[String] =
    //еще раз, что это? это помещение какого то значения в IO
      IO.pure("какой то файл, содержимое")

    def httpPost(url: String, body: String): IO[Unit] =
    //еще раз, что это? Приостанавливает синхронный побочный эффект в IO.
      IO.delay(println(s"POST '$url': '$body'"))

    def run: IO[Unit] = for {
      _ <- IO.delay(println("enter file path"))
      path <- IO.readLine
      data <- readFile(path)
      _ <- httpPost("http:sdfsdf.de", data)
    } yield ()
  }
}

//перепишем в taglessFinal
trait FileSystem[F[_]] {
  def readFile(path: String): F[String]
}

object FileSystem {
  //это суммонеры, это позволяет быстро получить implicit filesystem,
  // благодаря этому можно FileSystem[F].readFile()
  // implicitly это указание компилатяору вернуть доступный имплисит
  def apply[F[_] : FileSystem]: FileSystem[F] = implicitly
}

trait HttpClient[F[_]] {
  def postData(url: String, body: String): F[Unit]
}

object HttpClient {
  def apply[F[_] : HttpClient]: HttpClient[F] = implicitly
}

// надо написать DSL для консоли, она должна уметь читать и что то напечатать
trait Console[F[_]] {
  def readLine: F[String]
  def printLine(s: String): F[Unit]
}

object Console {
  def apply[F[_] : Console]: Console[F] = implicitly
}

object Interpreters {
  implicit val consoleIO: Console[IO] = new Console[IO] {
    def readLine: IO[String] = IO.readLine

    def printLine(s: String): IO[Unit] = IO.println(s)
  }

  // теперь интерпретатор
  implicit val fileSystemIO: FileSystem[IO] = new FileSystem[IO] {
    def readFile(path: String): IO[String] = IO.pure(s"some file with some content $path")
  }


  implicit val httpClientIO: HttpClient[IO] = new HttpClient[IO] {
    def postData(url: String, body: String): IO[Unit] = IO.delay(println(s"POST '$url': '$body'"))
  }
}

// склеиваем
object FilesAndHttpTF extends IOApp.Simple{
  // все описываем только F[_] и в конце добавляем движок [IO]
  // тут перейти к монадическим эффектам, так как без map flatMap это не скомпилируется
  //монада это констуркция для связывания эффектов
  //мы хотим тут выполнить один эффект, затем второй эффект trait Monad[F[_]] если есть map flatMap
  // поэтому надо добавить монаду
  def program[F[_]: Console: Monad: FileSystem: HttpClient]: F[Unit] =
    for {
      _ <- Console[F].printLine("Enter file path: ")
      path <- Console[F].readLine
      data <-FileSystem[F].readFile(path)
      _ <- HttpClient[F].postData("sdfsdf.de", data)

    } yield ()

  /*
    _ <- IO.delay(println("enter file path"))
      path <- IO.readLine
      data <- readFile(path)
      _ <- httpPost("http:sdfsdf.de", data)
  */

  import catseffect.Interpreters.consoleIO
  import catseffect.Interpreters.fileSystemIO
  import catseffect.Interpreters.httpClientIO
  def run: IO[Unit] = program[IO]
}
