import sbt._

object Dependencies {
  val scalazVersion = "7.2.26"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaz ="org.scalaz" %% "scalaz-core" % scalazVersion
  lazy val scalazEffect ="org.scalaz" %% "scalaz-effect" % scalazVersion
}
