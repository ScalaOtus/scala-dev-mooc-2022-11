package catsstreamingjson

import cats.effect.{IO, IOApp}
import catsmiddleware.Restfull.{addResponseHeaderMiddleware, routerSessionsAuthClear, serviceOne, serviceTwo}
import catsstreamingjson.CirceJson.User
import catsstreamingjson.HttpClient.{builder, request}
import io.circe.{Decoder, Json, ParsingFailure}
import io.circe.parser.parse
import org.http4s.Method.POST
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.server.AuthMiddleware
import org.http4s.{AuthedRoutes, Http, HttpRoutes, Request, Uri}
//import io.circe.generic.semiauto._
import io.circe.generic.auto._
//import io.circe.derivation._
import cats.Functor
import cats.conversions.all.autoWidenBifunctor
import cats.data.{Kleisli, OptionT}
import cats.effect._
import com.comcast.ip4s.{Host, Port}
import org.http4s.{AuthedRequest, AuthedRoutes, Http, HttpRoutes, MediaType, Method, Request, Response, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.server.{AuthMiddleware, HttpMiddleware, Router}
import org.typelevel.ci.CIStringSyntax
import cats.implicits._
import org.http4s.LiteralSyntaxMacros.uri
import org.http4s.implicits.http4sLiteralsSyntax

object CirceJson extends IOApp.Simple{

  case class User(name: String, email: Option[String])
  case class Permissions(user: User, id: Int)

  //1. декодер для User
  /*implicit val decoderUser: Decoder[User] = Decoder.instance(
    cur =>
      for {
        name <- cur.downField("name").as[String]
        email <- cur.downField("email").as[Option[String]]
      } yield User(name, email)
  )*/

  //2. semiauto
//  implicit val decoderUser: Decoder[User] = deriveDecoder
//  implicit val decoderPermissions: Decoder[Permissions] = deriveDecoder


  val example = """{"name": "xxxxx", "email": "dsfsdfs@sdfsdf.de"}"""
  val json: Either[ParsingFailure, Json] = parse(example)


  def run: IO[Unit] = IO.println{
    for {
      json <- parse(example)
      user <- json.as[User]

    } yield user
  }
}


import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._

object Restfuldesc{

  def publicRoutes: HttpRoutes[IO] = HttpRoutes.of {
    case r @ POST -> Root/"echo" =>
      {
        for {
          u <- r.as[CirceJson.User]
          _ <- IO.println(u)
          response <- Ok(u)

        } yield  response


      }



  }

  val router = Router("/public" -> publicRoutes)

  val server = for {
  s <- EmberServerBuilder
    .default[IO]
    .withPort(Port.fromInt(8080).get)
    .withHost(Host.fromString("localhost").get)
    .withHttpApp(router.orNotFound).build
  }yield s

}

object HttpClientCirce {
  val builder = EmberClientBuilder.default[IO].build
  val postrequest = Request[IO](
    method = Method.POST,

    uri = Uri.fromString("http://localhost:8080/public/echo").toOption.get)
    .withEntity(User("test", Some("edrfgdfg@dxfgxfgfd.de")))

  val result = builder.use(
    client => client.run(postrequest).use(
      resp => if (resp.status.isSuccess)
        resp.as[User]
      else
        IO.raiseError(new Exception("error"))

    ))
}


object MainCircePars extends IOApp.Simple{
  def run : IO[Unit] ={
  for {
    _ <- Restfuldesc.server.use( _=>
    HttpClientCirce.result.flatMap(IO.println) *> IO.never
    )
  } yield ()
  }

}