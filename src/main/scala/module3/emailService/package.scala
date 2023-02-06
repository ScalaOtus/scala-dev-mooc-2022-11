package module3

import zio.Has
import zio.{UIO, URIO}
import zio.{ULayer, ZLayer}
import zio.console
import zio.ZIO
import zio.console.Console


package object emailService {

    /**
     * Реализовать Сервис с одним методом sendEmail,
     * который будет принимать Email и отправлять его
     */

    type EmailService = Has[EmailService.Service] // 1

    object EmailService {
        trait Service{
            def sendMail(email: Email): URIO[zio.console.Console, Unit]
        }

        val live = ZLayer.succeed(
            new Service {
                override def sendMail(email: Email): URIO[Console, Unit] =
                    zio.console.putStrLn(email.toString).orDie
            }
        )

        def sendMail(email: Email): URIO[EmailService with zio.console.Console, Unit] =
            ZIO.accessM(_.get.sendMail(email))
    }

}
