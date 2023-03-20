package catsmiddleware

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
import catsmiddleware.Restfull.httpApp
import org.http4s.LiteralSyntaxMacros.uri
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._

object  Restfull {
  //1

  val service: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello"/ name => Ok("bla bla bla")
    }

  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello1"/ name => Ok("bla1 bla1 bla1")
    }
  val serviceTwo: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello2"/ name => Ok("bla2 bla2 bla2")
    }

  //2
  val routes = addResponseMiddleware(Router("/" -> addResponseMiddleware(serviceOne), "/api" -> addResponseMiddleware(serviceTwo)))

  val httpApp: Http[IO,IO] = service.orNotFound

  val server = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(httpApp).build
  } yield s
  //2
  def addResponseMiddleware[F[_]: Functor](
                                          routes: HttpRoutes[F]
                                          ): HttpRoutes[F] = Kleisli{
    req =>
      val maybeResponse = routes(req)
//      maybeResponse.map(resp => resp.putHeaders("X-Otus" -> "Hello"))

      maybeResponse.map{
        case Status.Successful(resp) => resp.putHeaders("X-Otus" -> "Hello")
        case other => other
      }
  }
  // import org.http4s.server.middleware._

  val server1 = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routes.orNotFound).build
  } yield s

  //3. Сессии
  type Sessions[F[_]] = Ref[F, Set[String]]
  def serviceSessions(sessions: Sessions[IO]): HttpRoutes[IO]=
    HttpRoutes.of{
      case r@GET -> Root / "hello" =>
        r.headers.get(ci"X-User-Name") match{
          case Some(values) =>
            val name = values.head.value
            sessions.get.flatMap(users =>
              if (users.contains(name)) Ok(s"Hello, $name")
              else
                Forbidden("You shall not pass!!!")
            )
          case None => Forbidden("You shall not pass!!!")
        }
      case PUT -> Root / "login" / name =>
        sessions.update(set => set + name).flatMap( _ => Ok("done"))
    }

  def roterSessions(sessions: Sessions[IO]) =
    addResponseMiddleware(Router("/" -> serviceSessions(sessions)))

  val serverSessions = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(roterSessions(sessions).orNotFound).build
  } yield s

  //4. Auth
  def serviceAuth(sessions: Sessions[IO]): HttpMiddleware[IO] =
    routes =>
    Kleisli{ req =>
      req.headers.get(ci"X-User-Name") match{
        case Some(values) =>
          val name  = values.head.value
          for{
            users <- OptionT.liftF(sessions.get)
            results <-
              if (users.contains(name)) routes(req)
              else OptionT.liftF(Forbidden("You shall not pass!!"))
          } yield results
        case None => OptionT.liftF(Forbidden("You shall not pass!!"))
      }
    }

  def serviceHello: HttpRoutes[IO] =
    HttpRoutes.of {
      case r@GET -> Root / "hello" =>
        r.headers.get(ci"X-User-Name") match {
          case Some(values) =>
            val name = values.head.value
            Ok(s"Hello, $name")
          case None =>  Forbidden("You shall not pass!!")
        }
    }

  def loginService(sessions: Sessions[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case PUT -> Root / "login" / name =>
        sessions.update(set => set + name).flatMap(_ => Ok("done"))
    }

  def routerSessionAuth(sessions: Sessions[IO]) =
    addResponseMiddleware( Router("/" -> (loginService(sessions) <+> serviceAuth(sessions)(serviceHello))) )

  val serverSessionsAuth = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessionAuth(sessions).orNotFound).build
  } yield s

  //5 refactoring
  final case class User(name: String)
  def serviceHelloAuth: AuthedRoutes[User, IO] = AuthedRoutes.of {
    case GET -> Root / "hello" as user =>
      Ok(s"Hello, ${user.name}")
  }

  def serviceAuthMiddleware(sessions: Sessions[IO]): AuthMiddleware[IO, User] =
    autherRoutes =>
      Kleisli { req =>
        req.headers.get(ci"X-User-Name") match {
          case Some(values) =>
            val name = values.head.value

            for {
              users <- OptionT.liftF(sessions.get)
              results <-
                if (users.contains(name)) autherRoutes(AuthedRequest(User(name), req))
                else OptionT.liftF(Forbidden("You shall not pass!!"))
            } yield results
          case None => OptionT.liftF(Forbidden("You shall not pass!!"))
        }
      }
  def routerSessionsAuthClear(sessions: Sessions[IO])=
    addResponseMiddleware( Router("/" -> (loginService(sessions) <+> serviceAuthMiddleware(sessions)(serviceHelloAuth))))

  val serverSessionsAuthClear = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessionsAuthClear(sessions).orNotFound).build
  } yield s

}

object mainServer extends IOApp.Simple{
  def run(): IO[Unit] ={
    //1
    // Restfull.server.use(_ => IO.never)
    //2
    //Restfull.server1.use(_ => IO.never)
    //3
    //Restfull.serverSessions.use(_ => IO.never)
    //4
    //Restfull.serverSessionsAuth.use(_ => IO.never)
    //5
    Restfull.serverSessionsAuthClear.use(_ => IO.never)

  }
}

//6 tests
object Test extends IOApp.Simple{

  def run: IO[Unit] ={
    val service = Restfull.serviceHelloAuth
    for {
      result <- service(AuthedRequest(Restfull.User("abc"), Request(method = Method.GET,
        uri = Uri.fromString("/hello").toOption.get))).value
      _ <- result match {
        case Some(resp) =>
          ???
        case None => ???
      }}


    }




  }

}

