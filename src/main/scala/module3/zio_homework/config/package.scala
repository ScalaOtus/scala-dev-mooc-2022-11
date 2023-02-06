package module3.zio_homework

import zio.config.ReadError
import zio.config.ReadError.SourceError
import zio.config.typesafe.TypesafeConfig
import zio.{Cause, Has, IO, Layer, Task, ZIO}


package object config {
   case class AppConfig(host: String, port: String)

  import zio.config.magnolia.DeriveConfigDescriptor.descriptor

  val configDescriptor = descriptor[AppConfig]

  type Configuration = zio.Has[AppConfig]

  object Configuration{
    val live: Layer[ReadError[String], Configuration] = TypesafeConfig.fromDefaultLoader(configDescriptor)
  }

  def load: IO[ReadError[String], AppConfig] = ZIO.accessM[Configuration](c => ZIO.effect(c.get)
    .orElseFail(SourceError("Can't get config from file"))).provideLayer(Configuration.live)
}
