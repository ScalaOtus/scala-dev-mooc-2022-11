package module3.e20_catseffect_01

// todo: console

trait FileSystem[F[_]] {
  def readFile(path: Path): F[String]
}
object FileSystem {
  def apply[F[_]: FileSystem]: FileSystem[F] = implicitly
}

trait HTTPClient[F[_]] {
  def postData(url: URI, body: String): F[Unit]
}
object HTTPClient {
  def apply[F[_]: HTTPClient]: HTTPClient[F] = implicitly
}
