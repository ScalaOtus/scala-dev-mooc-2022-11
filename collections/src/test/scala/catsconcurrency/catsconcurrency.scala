package catsconcurrency

import cats.implicits.*
import cats.effect.{Deferred, IO, IOApp, Ref, Resource}
import cats.Alternative.ops.toAllAlternativeOps

import scala.util.Try
import cats.syntax.either.*

// большинство типов данных лежит в cats.std это то что смотрели refs итд
// сегодня будем смотреть как из кирпичиков cats собирать приложения
// IO мы уже знаем, это просто вычисление, потенциально не ссылочно прозрачное внутри (У ней внутре неонка)
// Resource  - Любая вещь, требующая аллокации ресурсов. Это память, потоки, коннекшены

//Итак, Deferred — это отложенный результат, результат выполнения, который станет известен через некоторое время.
// Результатом, хранящимся в Deferred, может быть произвольное значение (успешное выполнение) или ошибка (исключение),
// которое произошло в процессе выполнения асинхронной операции.


//Resource
// дает функциональный способ try catch finally
// make это создание ресурса, 2 параметра 1. способ как получаем ресурс (эффект, не ссылчно прозрачный скорее всего это A)
// и второй параметр это то, как ресурс отдаем, это тоже сайд эффект
// в примере файл будет жить толькго внутри use. тоесть его жизнь ограничена этой лямбда


//Ref
//это функциональная атомаргная ссылка на ячейку памяти
//потерянное обновление (англ. lost update) — при одновременном изменении одного блока данных разными транзакциями теряются все изменения, кроме последнего;
//«грязное» чтение (англ. dirty read) — чтение данных, добавленных или изменённых транзакцией, которая впоследствии не подтвердится (откатится);
//неповторяющееся чтение (англ. non-repeatable read) — при повторном чтении в рамках одной транзакции ранее прочитанные данные оказываются изменёнными;
//фантомное чтение (англ. phantom reads) — одна транзакция в ходе своего выполнения несколько раз выбирает множество строк по одним и тем же критериям. Другая транзакция в интервалах между этими выборками добавляет строки или изменяет столбцы некоторых строк, используемых в критериях выборки первой транзакции, и успешно заканчивается. В результате получится, что одни и те же выборки в первой транзакции дают разные множества строк.
//Рассмотрим ситуации, в которых возможно возникновение данных проблем.
//Важно то, что создание REF это эффект

//Deferred это одноразовая ячейка памяти и создатся пустой


//Fiber
//start это не метод fiber Это метод IO который. Вызывая стрт мы создаем еще одно IO которое не будет
//делать вычисление и ждать его конца, отправит это вычисление на fiber и вернет этот fiber
//пример - есть какое то вычисление на 100 000 000 операций, если сделаем flatmap то буудем ждать
//а если вызовем .start то отправим на fiber и продолжим вычисления

object MainCatsConcurrent extends IOApp.Simple {

  //1 это показать сначала
  // это не в tagless final
  //сначала пишем команду echo и exit
  //сначала пишем считывание, помошь студентов IO.readLine
  //сначала через for
  def program: IO[Unit] = for {
    cmd <- IO.readLine
    _ <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(command)
    }
  } yield ()

  def process(command: Command): IO[Unit] =
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => IO.println(text))
      }
      case Command.Exit => {
        IO.println("Bye Bye")
      }
      case Command.AddNumber(num) => ???
      case Command.ReadNumber => ???
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }

  def run: IO[Unit] = program
  // тут запустить, рассказать про то, что комманды запускаются без цикла

}

/*
  //2 цикл команд
  //функция process должна что то возвращать, true продолжаем, false завершаем
  //теперь process возвращает true false
  def program: IO[Unit] = for {
    cmd <- IO.readLine
    _ <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(command).flatMap{
        case true => program // тут рекурсия
        case false => IO.unit //тут все понятно, мы Bye Bue уже напечатали, можно ничего не возвращать
      }
    }
  } yield ()

  def process(command: Command): IO[Boolean]=
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => IO.println(text)).as(true) //тоже самое как map(_ => true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => ???
      case Command.ReadNumber => ???
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }

  def run: IO[Unit] = program

*/

/*
  //3 убираем рекурсию
  //функция process должна что то возвращать, true продолжаем, false завершаем
  //теперь process возвращает true false
  //реализуем функционал со счетчиком
  def program(counter: Ref[IO, Int]) : IO[Unit] = iteration(counter).iterateWhile(a=>a).void

  def iteration(counter: Ref[IO, Int]) : IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProceed <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(counter)(command)
    }
  } yield shouldProceed

  def process(counter: Ref[IO, Int])(command: Command): IO[Boolean]=
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => IO.println(text)).as(true) //тоже самое как map(_ => true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => counter.updateAndGet(_ + num).flatMap(IO.println).as(true)
      case Command.ReadNumber => counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }

  //будет создавать все необходимые ресурсы, а program будет с ними запускаться
  def run: IO[Unit] = for{
    // тут надо указывть типы, эффект будет IO и Int
    counter <- Ref.of[IO, Int](0)
    _ <- program(counter)
  } yield ()
*/
/*
  //4 добавим паттерн окружения
  //функция process должна что то возвращать, true продолжаем, false завершаем
  //теперь process возвращает true false
  //реализуем функционал со счетчиком
  def program(env: Environment) : IO[Unit] = iteration(env).iterateWhile(a=>a).void

  def iteration(env: Environment) : IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProceed <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProceed

  def process(env: Environment)(command: Command): IO[Boolean]=
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => IO.println(text)).as(true) //тоже самое как map(_ => true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => env.counter.updateAndGet(_ + num).flatMap(IO.println).as(true)
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }

  //добавим паттерн окружения
  final case class Environment(counter: Ref[IO, Int])

  //будет создавать все необходимые ресурсы, а program будет с ними запускаться
  def run: IO[Unit] = for{
    // тут надо указывть типы, эффект будет IO и Int
    counter <- Ref.of[IO, Int](0)
    _ <- program(Environment(counter))
  } yield ()
*/

/*
  //5 делаем из окружения ресурс
  //функция process должна что то возвращать, true продолжаем, false завершаем
  //теперь process возвращает true false
  //реализуем функционал со счетчиком
  def program(env: Environment) : IO[Unit] = iteration(env).iterateWhile(a=>a).void

  def iteration(env: Environment) : IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProceed <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProceed

  def process(env: Environment)(command: Command): IO[Boolean]=
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => IO.println(text)).as(true) //тоже самое как map(_ => true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => env.counter.updateAndGet(_ + num).flatMap(IO.println).as(true)
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }

  //добавим паттерн окружения
  final case class Environment(counter: Ref[IO, Int])

  def buildEnv: Resource[IO, Environment] = {
    //указываем как ресурс аллоцируем и как отдаем
    val counter = Resource.make(IO.println("Alloc.  counter") *> Ref.of[IO, Int](0))(_ =>
      IO.println("Dealloc. counter")
    )

    counter.map(Environment)
  }


  //переписываем на ресурсы
  //как программа заверщится, все ресурсы освободятся
  def run: IO[Unit] =
    buildEnv.use(env => program(env) )
*/

  /*
  //6 запускаем собак, buildEnv ReleaseTheDogs LaunchDog
  //функция process должна что то возвращать, true продолжаем, false завершаем
  //теперь process возвращает true false
  //реализуем функционал со счетчиком
  def program(env: Environment) : IO[Unit] = iteration(env).iterateWhile(a=>a).void

  def iteration(env: Environment) : IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProceed <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProceed

  def process(env: Environment)(command: Command): IO[Boolean]=
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => IO.println(text)).as(true) //тоже самое как map(_ => true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => env.counter.updateAndGet(_ + num).flatMap(IO.println).as(true)
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) =>
    //пока без fiber
//    env.startGun.get *>
//        IO.println(s"Dog $name is starting") *> env.counter.updateAndGet(_ + 1)
//          .flatMap(value => IO.println(s"Dog $name observe value $value")).as(true)
    //какие тут проблемы? на get повиснем. Основной fiber ждет deffered, а окончить deffered можно только из основного потока
      // выход - отправить ожидающий код на отделоьный файбер
        val fiberIO = ( IO.println(s"Dog $name is ready") *> env.startGun.get *>
              IO.println(s"Dog $name is starting") *> env.counter.updateAndGet(_ + 1)
                .flatMap(value => IO.println(s"Dog $name observe value $value")))
        //создали файбер, отправили выполнять и готовы принимать новые команды
        fiberIO.start.as(true)
          //ЗАПУСК

      case Command.ReleaseTheDogs => env.startGun.complete(())
    }

  //добавим паттерн окружения
  //добавляем стартовый пистолет
  final case class Environment(counter: Ref[IO, Int], startGun:Deferred[IO,Unit])

  //здесь важно - порядок аллокации и деаллокации
  //gun может пользоваться counter потому никак нельзя деаллоцировать
  //counter пока живет gun это логика работы ресурсов
  //parTuple
  //parMapN .parMapN{ case (counter, gun) => Environment(counter, gun)}
  //  def buildEnv: Resource[IO, Environment] = (
  //counter <- Resource.make(IO.println("Alloc.  counter") *> Ref.of[IO, Int](0))(_ =>
  //      IO.println("Dealloc. counter")
  //    ),gun <- Resource.make(IO.println("Alloc.  gun") *> Deferred[IO, Unit])(
  //      _ => IO.println("Dealloc. gun"))
  //
  //   ).parMapN{ case (counter, gun) => Environment(counter, gun)} это на случай реальной независимости ресурсов
  // и порядок будет не детерминированным а с flatmap строгий порядок аллокации и деаллокации


  def buildEnv: Resource[IO, Environment] = for {
    //указываем как ресурс аллоцируем и как отдаем
    counter <- Resource.make(IO.println("Alloc.  counter") *> Ref.of[IO, Int](0))(_ =>
      IO.println("Dealloc. counter")
    )

    gun <- Resource.make(IO.println("Alloc.  gun") *> Deferred[IO, Unit])(
      _ => IO.println("Dealloc. gun"))

 } yield Environment(counter, gun)


  //переписываем на ресурсы
  //как программа заверщится, все ресурсы освободятся
  // launch-dog A
  // release-the-dogs
  //launch-dog B здесь Deferred уже выполнен
  def run: IO[Unit] =
    buildEnv.use(env => program(env) )
*/


  // 7 tagless final
  //функция process должна что то возвращать, true продолжаем, false завершаем
  //теперь process возвращает true false
  //реализуем функционал со счетчиком
 /* def program(env: Environment) : IO[Unit] = iteration(env).iterateWhile(a=>a).void

  def iteration(env: Environment[IO]) : IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProceed <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process[IO](env)(command)
    }
  } yield shouldProceed

  def process[F[_]:Console:Monad:Spawn ](env: Environment[F])(command: Command): F[Boolean]=
  //команда у нас есть, реалтзуем через паттерн матчинг ctrl alt space
    command match {
      case Command.Echo => {
        //считываем что ввел пользователь
        IO.readLine.flatMap(text => F.println(text)).as(true) //тоже самое как map(_ => true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => env.counter.updateAndGet(_ + num).flatMap(F.println).as(true)
      case Command.ReadNumber => env.counter.get.flatMap(F.println).as(true)
      case Command.LaunchDog(name) =>
        //пока без fiber
        //    env.startGun.get *>
        //        IO.println(s"Dog $name is starting") *> env.counter.updateAndGet(_ + 1)
        //          .flatMap(value => IO.println(s"Dog $name observe value $value")).as(true)
        //какие тут проблемы? на get повиснем. Основной fiber ждет deffered, а окончить deffered можно только из основного потока
        // выход - отправить ожидающий код на отделоьный файбер
        val fiberIO = ( F.println(s"Dog $name is ready") *> env.startGun.get *>
          IO.println(s"Dog $name is starting") *> env.counter.updateAndGet(_ + 1)
          .flatMap(value => F.println(s"Dog $name observe value $value")))
        //создали файбер, отправили выполнять и готовы принимать новые команды
        fiberIO.start.as(true)

      case Command.ReleaseTheDogs => env.startGun.complete(())
    }

  //добавим паттерн окружения
  //добавляем стартовый пистолет
  final case class Environment[F[_]](counter: Ref[F, Int], startGun:Deferred[IO,Unit])

  //здесь важно - порядок аллокации и деаллокации
  //gun может пользоваться counter потому никак нельзя деаллоцировать
  //counter пока живет gun это логика работы ресурсов
  //parTuple
  //parMapN .parMapN{ case (counter, gun) => Environment(counter, gun)}
  //  def buildEnv: Resource[IO, Environment] = (
  //counter <- Resource.make(IO.println("Alloc.  counter") *> Ref.of[IO, Int](0))(_ =>
  //      IO.println("Dealloc. counter")
  //    ),gun <- Resource.make(IO.println("Alloc.  gun") *> Deferred[IO, Unit])(
  //      _ => IO.println("Dealloc. gun"))
  //
  //   ).parMapN{ case (counter, gun) => Environment(counter, gun)} это на случай реальной независимости ресурсов
  // и порядок будет не детерминированным а с flatmap строгий порядок аллокации и деаллокации


  def buildEnv: Resource[IO, Environment] = for {
    //указываем как ресурс аллоцируем и как отдаем
    counter <- Resource.make(IO.println("Alloc.  counter") *> Ref.of[IO, Int](0))(_ =>
      IO.println("Dealloc. counter")
    )

    gun <- Resource.make(IO.println("Alloc.  gun") *> Deferred[IO, Unit])(
      _ => IO.println("Dealloc. gun"))

  } yield Environment(counter, gun)


  //переписываем на ресурсы
  //как программа заверщится, все ресурсы освободятся
  // launch-dog A
  // release-the-dogs
  //launch-dog B здесь Deferred уже выполнен
  def run: IO[Unit] =
    buildEnv.use(env => program(env) )
}

*/

sealed trait Command extends Product with Serializable

object Command {
  // команда на введение чего либо
  case object Echo extends Command
  // завершит работу
  case object Exit extends Command

  //инкрементный счетчик и сохранит состояние
  case class AddNumber(num: Int) extends Command
  //чтение этого состояния
  case object ReadNumber extends Command

  //эмуляция собачьих скачек
  case class LaunchDog(name: String) extends Command
  //запускаем собак, тут будем смотреть на fiber и Deferred
  case object ReleaseTheDogs extends Command

  // парсер
  def parse(s: String): Either[String, Command] =
    s.toLowerCase match {
      case "echo"             => Echo.asRight
      case "exit"             => Exit.asRight
      case "release-the-dogs" => ReleaseTheDogs.asRight
      case "read-number"      => ReadNumber.asRight
      case cmd =>
        cmd.split(" ").toList match {
          case List("launch-dog", dogName) =>
            LaunchDog(dogName).asRight
          case List("add-number", IntString(num)) =>
            AddNumber(num).asRight
          case _ =>
            s"Command $s could not be recognized".asLeft
        }
    }

  private object IntString {
    def unapply(s: String): Option[Int] =
      Try(s.toInt).toOption
  }
}

