package catsstreaming

import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource, SyncIO}
import fs2.{Chunk, Pure, Stream}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration._
import java.time.Instant

object Streams extends IOApp.Simple{

  //1
  val pureApply: Stream[Pure, Int] = Stream.apply(1,2,3)

  //2
  val ioApply: Stream[IO, Int] = pureApply.covary[IO]

  //3
  val list = List(1,2,3)
  val listEmits: Stream[Pure, Int] =  Stream.emits(list)

  //4
  val a: Seq[Int] = pureApply.toList

  //5
  val aa: IO[Unit] = ioApply.compile.drain

  //6
  val unfolded = Stream.unfoldEval(0){ s =>
    val next = s+10
    if (s >= 50) IO.none
    else IO.println(next.toString).as(Some(next.toString, next))
  }

  //7
  val s = Stream.eval(IO.readLine).evalMap(s => IO.println(s">> $s")).repeatN(3)

  //8
  type Descriptor = String
  def openFile: IO[Descriptor] =
    IO.println("open file").as("file descriptor")
  def closeFile(descriptor: Descriptor): IO[Unit] =
    IO.println("closing file")
  def readFile(descriptor: Descriptor): Stream[IO, Byte] =
    Stream.emits(s"File content".map(_.toByte).toArray)

  //9
  val fileResource: Resource[IO, Descriptor] = Resource.make(openFile)(closeFile)
  val resourceStream: Stream[IO,Int] = Stream.resource(fileResource).flatMap(readFile).map(b=>b.toInt + 100)

  val resourceStreamChunks: Stream[IO, Int] =
    Stream.resource(fileResource).flatMap(readFile).map(b=>b.toInt + 100).rechunkRandomly()


  //10 delay rate

  val fixedDelayStream = Stream.fixedDelay[IO](1.second).evalMap(_ => IO.println(Instant.now()))
  val fixedRateStream = Stream.fixedRate[IO](1.second).evalMap(_ => IO.println(Instant.now()))


  //11
  val queueIO = cats.effect.std.Queue.bounded[IO, Int](100)

  def putInQueue(queue: Queue[IO,Int], value:Int) =
    queue.offer(value)

  val queueStreamIO = for {
    q <- queueIO
    _ <- (IO.sleep(500.millis) *> putInQueue(q, 5)).replicateA(10).start
  } yield Stream.fromQueueUnterminated(q)


  val queueStream = Stream.force(queueStreamIO)



  def increment(s: Stream[IO, Int]): Stream[IO, Int] = s.map(_+1)


  def run: IO[Unit] ={

  //  queueStream.through(increment).compile.drain

    //(queueStream ++ queueStream).compile.drain
    //parevalmap

    resourceStream.parEvalMap(3)(b=>IO.sleep(500.millis) *> IO.println(b)).compile.drain



  }
//  delay
  /*
  2023-03-13T18:02:24.662209700Z
2023-03-13T18:02:25.675659700Z
2023-03-13T18:02:26.692735700Z
2023-03-13T18:02:27.694807300Z
2023-03-13T18:02:28.709203Z
   */

  /*
  rate
  2023-03-13T18:03:00.459458400Z
2023-03-13T18:03:01.441887700Z
2023-03-13T18:03:02.444969100Z
2023-03-13T18:03:03.442036700Z
2023-03-13T18:03:04.454570900Z
2023-03-13T18:03:05.442180900Z
2023-03-13T18:03:06.454047400Z
2023-03-13T18:03:07.455117Z
2023-03-13T18:03:08.443191300Z
2023-03-13T18:03:09.442658100Z
   */


}