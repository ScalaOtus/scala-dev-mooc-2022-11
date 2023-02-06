package module3.cats_effect_homework

import cats.Monad
import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import cats.implicits._
import module3.cats_effect_homework.Wallet._

// Здесь мы хотим протестировать бизнес-логику использующую кошельки: функцию transfer.
// Однако мы не хотим в наших тестах создавать какие-то файлы: в реальном приложении такой тест будет нуждаться в базе данных,
// что очень неудобно, ведь мы тестируем не сами кошельки а функцию transfer.

// Для таких тестов нам бы очень пригодился in-memory кошелёк, который хранит свой баланс в памяти пока работает тест.
// Он будет очень быстрым и дешевым - то что нужно для тестирования.

// Для такой задачи хорошо подойдет cats.effect.kernel.Ref - ячейка памяти с атомарным доступом.
// Нужно сделать интерпретатор Wallet[IO] который будет использовать Ref и запустить тест с помощью такого Wallet.
object WalletTransferApp extends IOApp.Simple {

  // функция, которую мы тестируем. Здесь менять ничего не нужно :)
  def transfer[F[_]: Monad](a: Wallet[F],
                            b: Wallet[F],
                            amount: BigDecimal): F[Unit] =
    a.withdraw(amount).flatMap {
      case Left(BalanceTooLow) => a.topup(amount)
      case Right(_)            => b.topup(amount)
    }

  // todo: реализовать интерпретатор (не забывая про ошибку списания при недостаточных средствах)
  final class InMemWallet[F[_]](ref: Ref[F, BigDecimal]) extends Wallet[F] {
    def balance: F[BigDecimal] = ???
    def topup(amount: BigDecimal): F[Unit] = ???
    def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = ???
  }

  // todo: реализовать конструктор. Снова хитрая сигнатура, потому что создание Ref - это побочный эффект
  def wallet(balance: BigDecimal): IO[Wallet[IO]] = ???

  // а это тест, который выполняет перевод с одного кошелька на другой и выводит балансы после операции. Тоже менять не нужно
  def testTransfer: IO[(BigDecimal, BigDecimal)] =
    for {
      w1 <- wallet(100)
      w2 <- wallet(200)
      _ <- transfer(w1, w2, 50)
      b1 <- w1.balance
      b2 <- w2.balance
    } yield (b1, b2)

  def run: IO[Unit] = {
    // 50, 250
    testTransfer.flatMap { case (b1, b2) => IO.println(s"$b1,$b2") }
  }

}
