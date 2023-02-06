package module3.e22_catseffect_03

import scala.util.Try

import cats.syntax.either._

sealed trait Command extends Product with Serializable

object Command {
  case object Echo extends Command
  case object Exit extends Command

  case class AddNumber(num: Int) extends Command
  case object ReadNumber extends Command

  case class LaunchDog(name: String) extends Command
  case object ReleaseTheDogs extends Command

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
