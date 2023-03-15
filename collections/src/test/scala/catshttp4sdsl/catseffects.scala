package catshttp4sdsl

import cats.Functor
import cats.data.{EitherT, OptionT, ReaderT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

object  catshttp4sdsl {
  //1.
  def getUserName: IO[Option[String]] = IO.pure(Some("dfb"))
  def getId(bname: String): IO[Option[Int]] = IO.raiseError(new Exception("some error"))
  def getPermissions(id: Int): IO[Option[String]] = IO.pure(Some("Permissions"))

/*
  def compose[F[_]: Functor, G[_]: Functor]: Functor[X=>F[G[_]]] = new Functor[X=>F[G[_]]]{
    override  def map[A,B](fa: F[G[A]])(f: A=>B): F[G[B]]=
      Functor[F].map(ga => Functor[G].map)
  }
  */

  def main (args: Array[String]): Unit ={
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

    // 1.
    val res: OptionT[IO, (String, Int, String)] = for {
      username <- OptionT(getUserName) //теперь getUserName стала монадой, благодаря обертке
      id <- OptionT(getId(username))
      permissions <- OptionT(getPermissions(id))
    } yield (username,id, permissions)

//    println(res.value.unsafeRunSync())
    //2.
    def getUserName1: IO[Option[String]] = IO.pure(Some("dfb"))
    def getId1(bname: String): IO[Int] = IO.pure(42)
    def getPermissions1(id: Int): IO[Option[String]] = IO.pure(Some("Permissions"))
    val res1 = for {
      username <- OptionT(getUserName1)
      id <- OptionT.liftF(getId1(username))
      permissions <- OptionT(getPermissions1(id))
    } yield (username,id, permissions)

    //3. EitherT
    sealed trait UserServiceError
    case class PermissionDenied(msg: String) extends UserServiceError
    case class UserNotFound(userId: Int) extends UserServiceError
    def getUserName2(userid: Int): EitherT[IO, UserServiceError, String]= EitherT.pure("test")

    def getUserAddress(userId: Int): EitherT[IO, UserServiceError, String] =
      EitherT.fromEither(Right("bla bla bla"))

    def getProfile(id:Int) = for{
      name <- getUserName2(id)
      address <- getUserAddress(id)
    } yield(name, address)

    //println(getProfile(2).value.unsafeRunSync())

    //4.
    // A=>B
    // Function[A,?]
    // a=> f(g(a))
    // A=>B ReaderT: A=>F[B]

    trait ConnectionPool
    case class Environment(cp: ConnectionPool)
    def getuserAlias(id:Int/*,env:Environment*/): ReaderT[IO, Environment, String] = ReaderT(cp => IO.pure("111"))
    def getComment(id:Int): ReaderT[IO, Environment, String] = ReaderT.liftF(IO.pure("222"))
    def updateComment(id:Int, text:String): ReaderT[IO, Environment, Unit] = ReaderT.liftF(IO.println("updated"))

    val result = for{
      a <- getuserAlias(1)
      b <- getComment(1)
      _ <- updateComment(1,"bla bla bla")
    } yield (a,b)

    println(result(Environment(new ConnectionPool {})).unsafeRunSync())











  }


}
