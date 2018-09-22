import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.lpld",
      scalaVersion := "2.12.6",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "simple-games",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaz,
    libraryDependencies += scalazEffect,
    scalacOptions ++= Seq(
      "-Ypartial-unification",
      "-Xfatal-warnings",
      "-language:higherKinds"
    )
  )
