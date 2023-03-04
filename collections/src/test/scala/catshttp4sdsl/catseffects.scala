package catshttp4sdsl

import cats.Functor
import cats.data.{EitherT, OptionT, ReaderT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

object  catshttp4sdsl {

  //напишем функцию, которая что то брет из реальгого
  //мира, но этого чего то там может не быть
  def getUserName: IO[Option[String]] = IO.pure(Some("sdf"))
  def getId(name: String): IO[Option[Int]] = IO.pure(Some(42))
  def getPermissions(int:Int): IO[Option[String]] = IO.pure(Some("Permissions"))

  //для функторов это будет работать
//  def compose[F[_]: Functor, G[_]: Functor]: Functor[X=> F[G[X]]] = new Functor[X => F[G[X]]]{
    //маппаем через 2 слоя
//    override def map[A,B](fa:F[G[A]])(f:A=>B): F[G[B]] =
//      Functor[F].map(ga => Functor[G].map(f:...))
    //это реалтзация композиции функторов в обшем виде, любые 2 функтора можно скомпозировать и композиция, что важно, тоже будет функтором
    //для монад такого нет
    //но каждая конкретная композиция монад, например option+ что то, тоесть с зафиксировнным 1 слоем, то результат будет
    //компоировтаься с любой другой монадой, это и заложено в монад трансформеры
    // тоесть на каждую монау, которую мы фиксируем есть трансформер
    //OptionT это трансформер для нашего примера с getPermissions
  // тоесть OptionT Это обертка для значения щавернутого в Option и какую то другую монаду
//  }



  @main def Ex(): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

    //теперь хотим их скомбинировтаь
    //но не компилируется, for разворачивает только первывй слой,
    //а слоев 2, для этого есть трансформер монад
    //    val res = for {
    //      username <- getUserName
    //      id <- getId(username)
    //    } yield()

    val res: OptionT[IO, (String, Int, String)] = for {
      username <- OptionT(getUserName) //теперь getUserName стала монадой, благодаря обертке
      id <- OptionT(getId(username))
      permissions <- OptionT(getPermissions(id))
    } yield (username,id, permissions)
    //тоесть еще раз OptionT это обертка до монады, посмотреть исходники

    println(res.value.unsafeRunSync()) // это уже IO от Option, если заменить что то на None то посмотреть на замыкание IO.pure(None)
    //2. уронить внешний слой IO.raiseError(new Exception("sdrfg"))

    //3.
    def getUserName1: IO[Option[String]] = IO.pure(Some("sdf"))
    def getId1(name: String): IO[Int] = IO.pure(42) //убираем Option
    def getPermissions1(int:Int): IO[Option[String]] = IO.pure(Some("Permissions"))
    val res1: OptionT[IO, (String, Int, String)] = for {
      username <- OptionT(getUserName1)
      id <- OptionT.liftF(getId1(username))//теперь не можем положииь в OptionT, нужен лифтинг, посмотреть исходники
      // IO.pure(42) => Some(42) И оспользовать fromOption[IO]
      permissions <- OptionT(getPermissions1(id))
    } yield (username,id, permissions)

    //4. EitherT
    //частый паттерн, когда ошщибки предметной области представляют в adt чтобы на них был паттерн матчинг
    sealed trait UserServiceError
    case class PermissionDenied(msg: String) extends UserServiceError
    case class UserNotFound(userId: Int) extends UserServiceError
    def getUserName2(userId:Int): EitherT[IO,UserServiceError, String] =
      EitherT.pure("sdg")

    def getUserAddress(userId:Int): EitherT[IO,UserServiceError, String] =
      EitherT.fromEither(Left(PermissionDenied("sdfsdfg")))

    def getProfile(id: Int) = for{
      name <- getUserName2(id)
      address <- getUserAddress(id)
    } yield (name, address)

    //IO не упало, но внутри LEFT, если положим right то выполнится нормально
    println(getProfile(2).value.unsafeRunSync())

    //5. ReaderT
    //чаще используется для композиции кода, для работы которого гужно какое то окружение
    // это развитие концепции, что функция тоже монада A => B это монада
    // Function[A,?] Это монада
    // a => f(g(a))это flatmap по функциям
    //readerT это развитие этой идеи
    //Reader: A=>B то ReaderT: A => F[B] посмотерть исходники, что ReaderT Это псевдоним на Kleisli

    trait ConnectionPool
    case class Environment(cp: ConnectionPool)
    // по сути это функция из Environemnt в IO[Sring]
    //но написана в виде, допускающем комбинацию
    def getUserAlias(id:Int): ReaderT[IO, Environment, String] = ReaderT(cp=>IO.pure("sdf"))
    def getComment(id:Int): ReaderT[IO, Environment, String] = ReaderT.liftF(IO.pure("sdf"))
    def updateComment(id:Int, text: String): ReaderT[IO, Environment, Unit] = ReaderT.liftF(IO.println("updated"))

    //теперь можем писать разные вычиления, какой результат?
    //вычисление зависит от Environemnt
    val result: ReaderT[IO,Environment, (String,String)] = for{
      a <- getUserAlias(1)
      b <- getComment(1)
      _ <- updateComment(1,"sdf")
    }yield (a,b)

    //тоесть это как бы экономия параметров, можно обойтись без readerT если getUserAlias(id:Int, env:Envoronment), тоесть ReaderT
    //выводит параметры в типы и фунцкии получаются чище и можно композировать
    //спойлер, сейчасэто не используется и замененно dependecyinjection, но знать полезно, как параметры могут скакать из
    //сигнатуры в параметры
    println(result(Environment(new ConnectionPool {})).unsafeRunSync())

    //теперь http4s
    
  }
}
