package catshttp4sdsl

import cats.data.Kleisli
import cats.effect._
import com.comcast.ip4s.{Host, Port}
import org.http4s.{Http, HttpRoutes, Request, Response}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router

object  Restfull {
  val service: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "hello" / name => Ok("web service OK")
  }

  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello1" / name => Ok(s"web service from $name")
      case POST -> Root / "hello2" / name => Ok(s"web service from $name")

    }

  val serviceTwo: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello2" / name => Ok("web service OK2")
    }

  val router  = Router(
    "/" -> serviceOne,
    "/api" -> serviceTwo,
    "/apiroot" -> service)

  val httpApp: Http[IO,IO] = service.orNotFound

  val server = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(httpApp).build
  } yield  s

  val server1 = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router.orNotFound).build
  } yield  s
}

object mainServer extends IOApp.Simple {
  def run() : IO[Unit] = {
    Restfull.server1.use( _ => IO.never)
  }
}