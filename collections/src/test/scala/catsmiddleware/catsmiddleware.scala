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
import catsmiddleware.Restfull.User
import org.http4s.LiteralSyntaxMacros.uri
import org.http4s.implicits.http4sLiteralsSyntax

object  Restfuldesc {
  // 1. reader если вернуться к ннотации F[_], то reader это функция A=>B где А это фиксирована, а B это та самая дырка
  //Kleisli позволяет композировать функции, которые возвращают монадическое значение, например, Option[Int]
  // или Either[String, List[Double]], без того, чтобы функции принимали Option или Both в качестве параметра,
  // что может быть странным и громоздким. У нас также может быть несколько функций, которые зависят от некоторой среды, и нам нужен хороший
  // способ скомпозировать эти функции, чтобы гарантировать, что все они получают одну и ту же среду. Или, возможно, у нас есть функции, которые
  // зависят от их собственной «локальной» конфигурации, и все конфигурации вместе составляют «глобальную» конфигурацию
  // приложения. Как сделать так, чтобы эти функции хорошо взаимодействовали друг с другом, несмотря на то,
  // что каждая из них знает только свои локальные требования? Именно в таких ситуациях Клейсли очень помогает.

  //2. middleware
  // type Middleware[F[_], A, B, C, D] = Kleisli[F, A, B] => Kleisli[F, C, D]
  AuthMiddleware

}

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

  //1.
  val router = addResponseHeaderMiddleware(Router("/" -> addResponseHeaderMiddleware(serviceOne), "/api" -> addResponseHeaderMiddleware(serviceTwo)))

  def routerSessions(sessions: Sessions[IO]) =
    addResponseHeaderMiddleware(Router("/" -> serviceSessions(sessions)))

  def routerSessionsAuth(sessions: Sessions[IO]) =
    //очень важно, это не коммутативно, нельзя написать наоборот, так как
    // тут задается порядок создания роутинга и если наоборот то попадем в middleware сначала
    addResponseHeaderMiddleware(Router("/" -> (LoginService(sessions) <+> serviceAuth(sessions)(serviceHello))))

  def routerSessionsAuthClear(sessions: Sessions[IO]) =
  //очень важно, это не коммутативно, нельзя написать наоборот, так как
  // тут задается порядок создания роутинга и если наоборот то попадем в middleware сначала
    addResponseHeaderMiddleware(Router("/" -> (LoginService(sessions) <+> serviceAuthMiddleware(sessions)(serviceHelloAuth))))


  ////////////////////////////добавим middleqware 1. в Taglessfinal

  //спросить и добавить Functor
  //так как map на OptionT и чтобы map работал надо чтобы F был функтором
  def addResponseHeaderMiddleware[F[_]: Functor](
                                       routes: HttpRoutes[F]
                                       ):HttpRoutes[F] =Kleisli{ req=>
      val maybeResponse = routes(req)
//    maybeResponse.map(resp => resp.putHeaders("X-Outus" -> "Hello"))
// или
    maybeResponse.map{
      case Status.Successful(resp) => resp.putHeaders("X-Outus" -> "Hello")
//      case Status.Successful(resp) => resp.putHeaders(`Content-Type`(MediaType.text.csv))
      case other  => other
    }
  } //теперь можем повесить на любой роут
    // val router = Router("/" -> addResponseHeaderMiddleware(serviceOne), "/api" -> serviceTwo)

    //2. middleware out of box
    //import org.http4s.server.middleware.

//3. Сессии
  type Sessions[F[_]]=Ref[F, Set[String]]
  def serviceSessions(sessions: Sessions[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case r@GET -> Root / "hello" =>
        //name будет в заголовке
        r.headers.get(ci"X-User-Name") match {
          case Some(values) => /*nonemptylist*/
            val name = values.head.value
            sessions.get.flatMap(users=>
            if (users.contains(name)) Ok(s"Hello, $name")
            else Forbidden("You will not pass!!!")
            )
          case None => Forbidden("You will not pass!!!")
        }
      case PUT -> Root / "login" / name =>
        sessions.update(set=>set + name).flatMap(_ => Ok("done"))
    }

  //4. Auth
  def serviceAuth(sessions: Sessions[IO]): HttpMiddleware[IO] =
    routes =>
      Kleisli { req =>
        req.headers.get(ci"X-User-Name") match {
          case Some(values) => /*nonemptylist*/
            //чтобы сощлись типы
            val name = values.head.value

            for {
              users <- OptionT.liftF(sessions.get)
              results <-
                if (users.contains(name)) routes(req)
                else OptionT.liftF(Forbidden("You will not pass!!!"))

            } yield results

          case None => OptionT.liftF(Forbidden("You will not pass!!!"))
        }
      }
  // теперь разобьем сервис на 2, в первом будет GET с аутентификацией, а второй будет с PUT

  def serviceHello: HttpRoutes[IO] =
    HttpRoutes.of {
      case r@GET -> Root / "hello" =>
        //name будет в заголовке
        r.headers.get(ci"X-User-Name") match {
          case Some(values) => /*nonemptylist*/
            val name = values.head.value
            Ok(s"Hello, $name")
          case None => Forbidden("You will not pass!!!")
        }
    }

  def LoginService(sessions: Sessions[IO]): HttpRoutes[IO]=
    HttpRoutes.of {
      case PUT -> Root / "login" / name =>
        sessions.update(set => set + name).flatMap(_ => Ok("done"))
    }

  //5. чистовой рефакторинг  AuthMiddleware
  //роут на который накладываем миддлваре не обычный, а аутентифицированный. тоесть внешний слой передает во внутрь доп данные

  final case class User(name: String)

  def serviceHelloAuth: AuthedRoutes[User, IO] = AuthedRoutes.of{
    case GET -> Root / "hello" as user =>
      Ok(s"Hello, ${user.name}")
  }

  def serviceAuthMiddleware(sessions: Sessions[IO]): AuthMiddleware[IO, User] =
    autherRoutes =>
      Kleisli { req =>
        req.headers.get(ci"X-User-Name") match {
          case Some(values) => /*nonemptylist*/
            //чтобы сощлись типы
            val name = values.head.value

            for {
              users <- OptionT.liftF(sessions.get)
              results <-
                if (users.contains(name)) autherRoutes(AuthedRequest(User(name), req))
                else OptionT.liftF(Forbidden("You will not pass!!!"))

            } yield results

          case None => OptionT.liftF(Forbidden("You will not pass!!!"))
        }
      }

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

  val serverSessions = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    //указываем эффект в котором запускаемся
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessions(sessions).orNotFound).build //и билдим сервер, это ресурс
  } yield s

  val serverSessionsAuth = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    //указываем эффект в котором запускаемся
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessionsAuth(sessions).orNotFound).build //и билдим сервер, это ресурс
  } yield s

  val serverSessionsAuthClear = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    //указываем эффект в котором запускаемся
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessionsAuthClear(sessions).orNotFound).build //и билдим сервер, это ресурс
  } yield s

}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    //    Restfull.server.use(_ => IO.never)

    //2.
   // Restfull.server1.use(_ => IO.never)
   //3.
  //  Restfull.serverSessions.use(_ => IO.never)
    //4.
//    Restfull.serverSessionsAuth.use(_ => IO.never)
    //5.
    Restfull.serverSessionsAuthClear.use(_ => IO.never)
  }

}

//5 тесты
object Test extends IOApp.Simple {

  def run: IO[Unit] = {
    val service = Restfull.serviceHelloAuth
    for {
      result <- service(AuthedRequest(User("abc"), Request(method = Method.GET,
        uri = Uri.fromString("/hello").toOption.get))).value
      _ <- result match {
        case Some(resp) =>
          resp.bodyText.compile.last.
            flatMap(body => IO.println(resp.status.isSuccess) *>
              IO.println(body))
        case None => IO.println("fail")
      }
    } yield ()
  }
}

