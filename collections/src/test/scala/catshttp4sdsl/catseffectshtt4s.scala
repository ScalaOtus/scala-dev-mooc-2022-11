package catshttp4sdsl

import cats.data.Kleisli
import cats.effect.*
import com.comcast.ip4s.{Host, Port}
import org.http4s.{Http, HttpRoutes, Request, Response}
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Router

object  Restfull {
  val service: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello" / name => Ok("dsg")
    }

    //правильно декомпозировтаь все роуты на кусочки
    //тоесть правильно писать какой то шлюз
    //который в зависимости от адреса перенапрявляет на разные адреса

  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello1" / name => Ok("dsg1")
    }
  val serviceTwo: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello2" / name => Ok("dsg2")
    }

  val router = Router("/" -> serviceOne, "/api" -> serviceTwo)



  val httpApp: Http[IO, IO] = service.orNotFound
  val server = for {
    //указываем эффект в котором запускаемся
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(httpApp).build //и билдим сервер, это ресурс
  } yield s

  val server1 = for {
    //указываем эффект в котором запускаемся
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router.orNotFound).build //и билдим сервер, это ресурс
  } yield s
}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
//    Restfull.server.use(_ => IO.never)

//2.
    Restfull.server1.use(_ => IO.never)
  }

}