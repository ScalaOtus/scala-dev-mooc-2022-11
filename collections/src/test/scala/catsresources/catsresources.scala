package catsresources

import cats.MonadError
import cats.data.State
import cats.implicits.*
import cats.effect.{Clock, Fiber, IO, IOApp, Spawn}
import cats.effect.unsafe.implicits.global
import cats.effect.kernel._

import scala.concurrent.duration.*

object catsresources {
  // monad error
  //самый простой пример это Option

  @main def catsresourcesEx(): Unit = {
    val optionF = for {
      a <- Some(3)
      b <- Some(3)
      c <- Some(3)
      d <- Some(3)
    } yield a + b + c + d



    //если вставим NOne то получим короткое замыкание и прерывание
    //монадических вычислений
    // но не увсех монад есть короткое замыкание, например State
    /*val optionF1 = for{
    a <- Right(3)
    b <- Right(3)
    c <- Left("error")
    d <- Right(3)
  } yield a + b + c + d
*/

    val stateF = for {
      a <- State { (s: Int) => (s + 1, s) }
      b <- State { (s: Int) => (s + 1, s) }
      c <- State { (s: Int) => (s + 1, s) }
      d <- State { (s: Int) => (s + 1, s) }
    } yield (s"$a + $b + $c + $d")

    println(stateF.runA(10).value)


    // теперь опишем monaderror
    //MonadError[_,_]  2 параметра, зафиксируем строку вместо одной из дырок
    //у него 2 параметра, один для ошибки, потому надо указать, самый простой способ написать алиас
    type MyMonadError[F[_]] = MonadError[F, String]
    def withErrorHandling[F[_]: MyMonadError] = for {
      //pure поднимает любое значение в application functor
      a <- MonadError[F, String].pure(10)
      b <- MonadError[F, String].pure(10)
      c <- MonadError[F, String].pure(10)
      d <- MonadError[F, String].pure(10)
    } yield (a + b + c + d)

    // 1. в какую ошибку упадет Option MonadError[Option, ?]  ? будет Unit  мы не можем в эту ошибку ничего не положить
    // shift ctr p
    MonadError[Option, Unit]

    //2. какой тип мы можем взять, чтобы ошибка была string?
    // нам нужен эффект в которм мы можем выполнить код withErrorHandling ??? Either
    type StringError[A] = Either[String, A]
    println(withErrorHandling[StringError])

    //теперь как кинуть ошибку
    def withErrorHandling1[F[_]: MyMonadError]: F[Int] = for {
      //pure поднимает любое значение в application functor
      a <- MonadError[F, String].pure(10)
      b <- MonadError[F, String].pure(10)
      c <- MonadError[F, String].raiseError[Int]("fail")
      d <- MonadError[F, String].pure(10)
    } yield (a + b + c + d)
    println(withErrorHandling1[StringError])

    //MonadError это функциональный try catch
    //2 способа обработать ошибку
    //1.hadleError это catch
    println(withErrorHandling1.handleError(error => 42))

    //можно сделать в общем виде
    def withErrorHandling2[F[_]: MyMonadError] = for {
      //pure поднимает любое значение в application functor
      a <- MonadError[F, String].pure(10)
      b <- MonadError[F, String].pure(10)
      c <- MonadError[F, String].raiseError[Int]("fail")
        .handleError(error => 42)
      d <- MonadError[F, String].pure(10)
    } yield (a + b + c + d)
    println(withErrorHandling2.handleError(error => 42))

    //2 метод attempt
    // Он работает так - ели ошибка, он кладет ее слева и не прерывает вычисления никогда
    def withErrorAttempt[F[_]: MyMonadError]: F[Either[String, Int]] =
      withErrorHandling1[F].attempt

    //например, тоетсь мы не падаем, а работаем с ошибкой как с данными
    val nonFailing: IO[Either[Throwable, Nothing]] = IO.raiseError(new Exception("fail") ).attempt

    //можно завернуть в IO то что падает
    val failing = IO.raiseError(new Exception("fail") )
    //тут println не вызовется, так как failing упал
    failing *> IO.println("sdgf")
    //но можно так, чтобы добраться до println
    failing.attempt *> IO.println("sdfg")


    // monadcancel
    //есть ошибки, есть отмены, они ортогональны
    // допустим есть функция getUser которая разными путями возвращает пользователя
    //все 3 запроса отправлены параллельно
    //и в кэше пользователь нашелся
    // то вторые 2 задачи надо отменить
    //MonadCancel это про отмену вычислений
    //второй случай это мы собираем сложный обьект
    //и мы параллельно загружаем, но если где то произошла ошибка
    //остальное надо отменить
    //тоетсь отмена не оштбка, но мы говорим, что результат уже не нужен

    //это последовательное выполнение без использования промежуточного результата
    val a = IO.println(42) *> IO.raiseError(new Exception("dfg")) *> IO.println(10)
    //a.unsafeRunSync()

    //monadcancel добавляет метод forceR который как productR но не прерывается на ошибке, только на сигнале отмены

    val b = IO.println(42) !> IO.raiseError(new Exception("dfg")) !> IO.println(10)
    b.unsafeRunSync()

    //это было формальное определение, для реальной демонстрации нужны fiber, Это чуть дальше
    val justSleep = IO.sleep(1.second) *> IO.println("not cancelled")
    val justSleepAndThrow = IO.sleep(100.millis) *> IO.raiseError(new Exception("error"))

    //запускам параллельно, одно упадет до того, как закончится второе
    //в cats параллельные вычисления написаны так, что если мы пачкой запускаем вычисления и одно падает, то остальные отменяются
    //не напечаталось ничего
    //(justSleep, justSleepAndThrow).parTupled.unsafeRunSync()

    //есть метод uncancillable
    val justSleepUncancelable = (IO.sleep(1.second) *> IO.println("not cancelled")).uncancelable
    (justSleepUncancelable, justSleepAndThrow).parTupled.unsafeRunSync()

    //итак parTuple говорит, что нужно все, результат не будет успешным если все не возвращено
    //если нужен хотя бы один результат, есть метод racePair
    //верни первый успех, остальное отмени
    //IO.racePair(justSleep, justSleepAndThrow)

    //clock random unique - вот все они генерируют функциональные эффекты
    //clock это функциональные часы
    //realtime возвращает текущее время. Мы его дергаем несколько раз и получаем разные значения тоесть не ссылчно прозрачный
    //если используем функциональные часы и пишем тесты, то можем подложить часы, которыми можем управлять
    //тоже самое random


    //sync синхронный cats эффект
    //ключевой метод это delay - это вычисление, тоесть заверни в IO и выполни когда понадобится
    //sync показать на дз
    // Wallet 27
    // будет синк так как будем работать с файлами и эффекты надо заворачивать в F
    // Sync[F].delay()


  }
}

//Spawn
// это про fiber
object SpawnApp extends IOApp.Simple{
  //будем запускать fiber и смотреть поведение
  def longRunningIO: IO[Unit]=
    (IO.sleep(200.millis) *>
      IO.println(s"Hi from ${Thread.currentThread}"))
      .iterateWhile(_ => true)
  // или рекурсией fllatMap(longRunningIO)

  /* def run: IO[Unit] = {
     //получаем фибер, с которым что то можно сделать, например отменить
     val fiber: IO[Fiber[IO, Throwable, Unit]] = Spawn[IO].start(longRunningIO)
   }*/
  //теперь в for comprehansion завернем

  //run тоже выполняется на fiber
  def run: IO[Unit] = for {
    fiber <- Spawn[IO].start(longRunningIO)
    //тоесть взяли код и скинули на другой поток, тоесть старт не будет выполнять код
    //и скажет - запусти на другом фвйбере, а я дальше буду работать
    //и если написать следующее, то оно выполнится
    _ <- IO.println("the fiber has been started and I'm still alive")
    _ <- IO.sleep(5.second)
  } yield ()


  //теперь делаем 3 fiber, переименовать его в run
  def run13fiber: IO[Unit] = for {
    fiber1 <- Spawn[IO].start(longRunningIO)
    fiber2 <- Spawn[IO].start(longRunningIO)
    fiber3 <- Spawn[IO].start(longRunningIO)

    //тоесть взяли код и скинули на другой поток, тоесть старт не будет выполнять код
    //и скажет - запусти на другом фвйбере, а я дальше буду работать
    //и если написать следующее, то оно выполнится
    _ <- IO.println("the fiber has been started and I'm still alive")
    _ <- IO.sleep(2.second)
    // И теперь можем отменить
    _ <- fiber1.cancel
    _ <- fiber2.cancel
    _ <- IO.sleep(3.second)
  } yield ()

  def longRunningIORef(r: Ref[IO, Int]): IO[Unit]=
    (IO.sleep(200.millis) *>
      IO.println(s"Hi from ${Thread.currentThread}"))
      .iterateWhile(_ => true)

  //итак пишем пусть и простые но параллельные вычисления на файберах, уже хорошо
  //показать реализацию Spawn
  //GenSpawn  сделан для возможной интеграции с zio
  // в zio ошибка может быть любого типа, а в cats это намертво throwable
  //и есть библиотеки позволяющие запускать cats на zio и там и спользуется
  // GenSpawn где ошибка вариативная

  //concurrent
  // Он вводит ресурс к которому обращаются несколько fiber
  //тоесть он вводит понятие конкурентного мутабельного состояния
  //тоесть оно может быть изменено и прочитано и причем безопасно
  //
  //теперь делаем concurrency fiber, переименовать его в run
  def runconcurrency: IO[Unit] = for {
    r <- Ref.of[IO, Int](10)
    fiber1 <- Spawn[IO].start(longRunningIORef(r))
    fiber2 <- Spawn[IO].start(longRunningIO)
    fiber3 <- Spawn[IO].start(longRunningIO)
    //REF это яяейка пямяти в которой
    //если ее прокинуть в fiber то он будет читать атомарно
    //это аналог java AtomicReference
    //это означает что доступ будет сделан так, как если никто другой
    //не имеет доступа, как наивысшая изоляция транзакций в БД
    //тоесть взяли код и скинули на другой поток, тоесть старт не будет выполнять код
    //и скажет - запусти на другом фвйбере, а я дальше буду работать
    //и если написать следующее, то оно выполнится
    //ref идет из type class concurrent GenConcurrent
    _ <- IO.println("the fiber has been started and I'm still alive")
    _ <- IO.sleep(2.second)
    // И теперь можем отменить
    _ <- fiber1.cancel
    _ <- fiber2.cancel
    _ <- IO.sleep(3.second)
  } yield ()

  //temporal создаем эффект который ничего не делает какое то время
  //важно понимать, что он бесплатен, так как на fiber
  //Temporal


  //Async самый мощный, возможность интеграции с кодом работающим через callback
  //это разбирали, нам runtime дает callback и мы его вызываем, тоесть вычисдение
  //с callback заворачивается в F  и у него свой ExecutionContext (пул потоков java)
  // Async
}
