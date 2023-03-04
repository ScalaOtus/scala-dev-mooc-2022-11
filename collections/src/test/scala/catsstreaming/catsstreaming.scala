package catsstreaming

import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource, SyncIO}
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global

import scala.concurrent.duration.*
import java.time.Instant

object Streams extends IOApp.Simple{

  //1.это чистый стрим, который в таком случае можно превратить в список без сайд эффектов
  // в apply список элементов, в случае с листом можно list:_*
  val pureApply: Stream[Pure, Int] = Stream.apply(1,2,3)//.toList //tolist это sinc в памяти

  //2.  тоесть расширяем stream до IO c помощью covary. можно и в zio
//  def f(s: Stream[IO, Int]) = ???
//  f(pureApply.covary)

  val ioApply: Stream[IO, Int] = pureApply.covary[IO]

  //3. еще один вариант конструктора emits
  val list = List (1,2,3,4)
  Stream.emits(list)

  //4
  val a: Seq[Int] = pureApply.toList
  //если на ioApply, кто скадет что тут будет?
  // ioApply.toList
  // тут ошибка компилятора так как не знаем как из IO выйти,
  // так как тут могут быть сайд эффекты.
  // тут на презентацию Sinks

  val aa: IO[List[Int]] = ioApply.compile.toList

  //5. стрим строчек, генерирующийся на основе интов
  // unfold работает только с Pure, unfoldeval с IO
  // суффиес Eval в fs2 означает версию функции работающей с сайдэффектами
  //тут запуситить
  val unfolded: Stream[IO, String] = Stream.unfoldEval(0){ s =>
    val next = s+10
    if (s >= 50) IO.none
    else IO.println(next.toString).as(Some((next.toString, next)))
  }

  //6. самый простой комбинатор для создания сайд эффектов
  //Stream.eval()
  // например будем делать стрим ввода пользователя и echo
  // evalMap комбинатор на стриме по преобразованию элемента с помощью F
  // тоесть аснхронный map с сайд эффектом
  val s = Stream.eval(IO.readLine).evalMap(s => IO.println(s">>$s")).repeatN(3)

  //7. resource
  // пример обработка файлов на стримах

  //это открытый файл, ресурс, который надо будет отдать
  type Descriptor = String
  def openFile: IO[Descriptor] =
    IO.println("open file").as("file descriptor")
  def closeFile(descriptor: Descriptor): IO[Unit] =
    IO.println("closing file")
  def readFile(descriptor: Descriptor): Stream[IO, Byte] =
    Stream.emits(s"File content".map(_.toByte).toArray)
  val fileResource: Resource[IO, Descriptor] = Resource.make(openFile)(closeFile)

  //этот конструктор сам правильно обработает аллокацию и деаллокацию
  //ресурс живет пока живет стрим
  //flatmap из каждого элемента создаем стрим
  val resourceStream: Stream[IO, Int] = Stream.resource(fileResource).flatMap(readFile).map(b=>b.toInt + 100)


  //8.chunks в презентацию
  //есть стрим байтов в файле п7
  //но прокидывать по байту трудозатратно
  //стримы оптимизируют процесс и под капотом элементы передаюися пачками chunks
  // тоесть можно этим управлять
 // val resourceStreamChunks: Stream[IO, Int] =
 // Stream.resource(fileResource).flatMap(readFile).map(b=>b.toInt + 100)
  //  .rechunkRandomly()
  //идея такая - мы пишем так, как будто элементы идут 1 за 1,а fs2
  //оптимизтрует чанками
  //но в evalMap мы не можем использовать чанки
  // так как там присутствует сайд эффект который может повлиять на обработку следующего
  //элемента и никакой порционности не будет
  //но Map можно
  //тоесть можно залезать в чанки,но не обязательно
  //но можно влезть, например mapChunkuns и что то делать сев ему на хвост

  //9. работа со временем
 // Stream.fixedRate()
  val fixedDelayStream = Stream.fixedDelay[IO](1.second)
   .evalMap(_ => IO.println(Instant.now))
  val fixedRateStream = Stream.fixedRate[IO](1.second)
    .evalMap(_ => IO.println(Instant.now))
 // они делают примерно одно и тоже, Delay берет какой то delay и раз в этот
  // duration генерирует unit, работает как метроном, кладет в стрим юнит
  //тоесть просто раз в какое то время в стрим кидает сигнал
  //fixRate fixDelay. разница в sleep посмотреть реализацию

  //10.
  //из очередей
  val queueIO = cats.effect.std.Queue.bounded[IO, Int](100)
  //10. Option[Int]
  def putInQueue(queue:Queue[IO,Int], value:Int)=
    queue.offer(value)

  //развернем очередь
  //_ <- (IO.sleep(500.millis) *> putInQueue(q, 5)).replicateA(10).start в очередб кладет
  //} yield Stream.fromQueueUnterminated() из очереди читает
  val queueStreamIO = for{
    q <-queueIO
    //создаем файбер
    _ <- (IO.sleep(500.millis) *> putInQueue(q, 5)).replicateA(10).start
 //   _ <- ((IO.sleep(500.millis) *> putInQueue(q, Some(5)).replicateA(10) *> q.offer(None)).start
    //теперь делаем стрим бесконечный
    //10.fromQueueNoneTerminated
  } yield Stream.fromQueueUnterminated(q)

  //и переводим StreamIO в стрим, так как стрим сильнее IO
  val queueStream = Stream.force(queueStreamIO)

  //11.
  def increment(s: Stream[IO, Int]): Stream[IO, Int] = s.map(_+1)

  //12.


  def run: IO[Unit] = {
    //5.альтернативы drain toList, Last итд, когда
    // надо не выбрасывать данные а скинуть их куда то
    //val res: IO[List[String]] = unfolded.compile.toList
    //drain запустить стрим без сохранения результата
    //другими словами drain это когда весь стрим интересен только из-за
    // сайд эффектов
    //unfolded.compile.drain

    //5. можем делать repeat
    //unfolded.repeat.compile.drain
    //unfolded.repeat.take(100).compile.drain

    //6.
    //s.compile.drain

    //7.
    //evalMap берем каждый элемент и на основе его производим еще один элемент
    //тоесть 1 элемент тв 1 элемент но с сайд эффектом
    // есть evalType это сайдд эффект и пробрасывение элеимента дальше(не меняя тип)
    //resourceStream.evalMap(IO.println).compile.drain

    //9.
    //fixedDelayStream.compile.drain

    //10.
    //queueStream.evalMap(IO.println).compile.drain
    //если сделаем terminated

    //11. есть комбинатор through это сахар с функцией из стрима в стрим
    //тоесть добавляем ступени преорбразования потоков, это по сути pipe
    queueStream.through(increment).compile.drain

    //конкатенация стримов
    (queueStream ++ queueStream).compile.drain
    //часто исподьзующийся комбинатор parevalmap
    //мы обрабатываем до 3х элементов параллельно
    //используется как оптимизатор, главное не указывать больше чем ядер на процессоре
    resourceStream.parEvalMap(3)(b=>IO.sleep(500.millis) *> IO.println(b)).compile.drain


  }
}