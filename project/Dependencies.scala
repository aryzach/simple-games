import sbt._

object Dependencies {
  val scalazVersion = "7.2.26"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaz ="org.scalaz" %% "scalaz-core" % scalazVersion
  lazy val zio = "org.scalaz" %% "scalaz-zio" % "0.2.7"
  lazy val zioInterop = "org.scalaz" %% "scalaz-zio-interop" % "0.2.7"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "1.0.0-M5"
  lazy val fs2Scalaz = "co.fs2" %% "fs2-scalaz" % "0.3.0"
  lazy val scalazEffect ="org.scalaz" %% "scalaz-effect" % scalazVersion
  lazy val scalazConcurrent ="org.scalaz" %% "scalaz-concurrent" % scalazVersion
  lazy val scalazStream = "org.scalaz.stream" %% "scalaz-stream" % "0.8.6"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.4.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.0.0"
  lazy val jansi = "org.fusesource.jansi" % "jansi" % "1.17.1"
  lazy val jline = "org.jline" % "jline" % "3.9.0"
}
