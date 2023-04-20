ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild /scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "collections"
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.5"
libraryDependencies += "co.fs2" %% "fs2-core" % "3.6.1"
libraryDependencies += "co.fs2" %% "fs2-io"   % "3.6.1"
libraryDependencies += "org.http4s" %% "http4s-client" % "0.23.18"
libraryDependencies += "org.http4s" %% "http4s-dsl" % "0.23.18"
libraryDependencies += "org.http4s" %% "http4s-ember-server" % "0.23.18"
libraryDependencies += "org.http4s" %% "http4s-ember-client" % "0.23.18"
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
libraryDependencies += "io.circe" %% "circe-derivation" % "0.13.0-M5"

libraryDependencies += "org.http4s" %% "http4s-circe" % "0.23.14"

lazy val akkaVersion = "2.7.0"
lazy val leveldbVersion = "0.7"
lazy val leveldbjniVersion = "1.8"

libraryDependencies ++= Seq(
  // Use Coda Hale Metrics and Akka instrumentation
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-query" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-sharding-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster-tools" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  "io.aeron" % "aeron-driver" % "1.40.0",
  "io.aeron" % "aeron-client" % "1.40.0",

  "org.iq80.leveldb" % "leveldb" % leveldbVersion,
  "org.fusesource.leveldbjni" % "leveldbjni-all" % leveldbjniVersion,

  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)

