package module3.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._

object WalletMain extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      wallet <- Wallet.fileWallet[IO]("test_wallet")
      _ <- wallet.topup(100.0)
      _ <- wallet.balance.flatMap(IO.println)
      _ <- wallet.withdraw(50.0)
      _ <- wallet.balance.flatMap(IO.println)
    } yield ()

}
