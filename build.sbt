import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.lpld",
      scalaVersion := "2.12.6",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "simple-games",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaz,
      fs2,
      cats,
      catsEffect
    ),
    scalacOptions ++= Seq(
      "-Ypartial-unification",
      "-Xfatal-warnings",
      "-language:higherKinds"
    )
  )
