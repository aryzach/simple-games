import sbt._

object Dependencies {
  val scalazVersion = "7.2.26"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaz ="org.scalaz" %% "scalaz-core" % scalazVersion
  lazy val zio = "org.scalaz" %% "scalaz-zio" % "0.2.7"
  lazy val zioInterop = "org.scalaz" %% "scalaz-zio-interop" % "0.2.7"
  lazy val scalazEffect ="org.scalaz" %% "scalaz-effect" % scalazVersion
  lazy val scalazConcurrent ="org.scalaz" %% "scalaz-concurrent" % scalazVersion
  lazy val scalazStream = "org.scalaz.stream" %% "scalaz-stream" % "0.8.6"
}
