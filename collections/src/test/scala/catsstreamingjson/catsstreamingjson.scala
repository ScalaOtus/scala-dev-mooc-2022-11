package catsstreamingjson

import cats.effect
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource, SyncIO}
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global
import catsmiddleware.Restfull
import org.http4s.client.Client
import org.http4s.{Request, Response, Uri}
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration._
import java.time.Instant

object HttpClient {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val request = Request[IO](uri = Uri.fromString("http://localhost:8080/hello").toOption.get)

  //1.
/*  val result = for {
    client <- builder
    respons <- client.run(request)
  } yield respons*/


  //2.
/*  val result = for {
    client <- builder
    response <-  effect.Resource.eval(client.expect[String](request))
  } yield response
*/

  //3.
  val result = builder.use(client => client.run(request).use(
    resp => if (!resp.status.isSuccess)
      resp.as[String]
    else
      IO("bla bla bla")
  ))

}

object  mainServer extends  IOApp.Simple{
  def run(): IO[Unit] ={

/*    for {
       fiber <- Restfull.serverSessionsAuthClear.use(_ => IO.never).start
       _ <- HttpClient.result.use(IO.println)
    } yield ()*/
    for {
      _ <- Restfull.serverSessionsAuthClear.use(_ => HttpClient.result.flatMap(IO.println) *> IO.never)
    }yield ()



  }
}




