package module3

import module3.emailService.EmailService
import module3.userDAO.UserDAO
import zio.{Has, ZIO}
import zio.ZLayer
import zio.console.Console
import zio.URIO
import module3.userService.{UserID, UserService}

object buildingZIOServices{


  val app: ZIO[UserService with EmailService with Console, Throwable, Unit] =
    UserService.notifyUser(UserID(1))

  val appEnv: ZLayer[Any, Nothing, UserService with EmailService] =
    UserDAO.live >>> UserService.live ++ EmailService.live

  def main(args: Array[String]): Unit = {
     zio.Runtime.default.unsafeRun(app.provideSomeLayer[Console](appEnv))
  }

}