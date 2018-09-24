package com.github.lpld.games

import cats.effect.{IO, Timer}
import fs2.Stream

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/**
  * @author leopold
  * @since 24/09/18
  */
trait ConsoleActions {

  def putStrLn(s: String) = IO { println(s) }

  val clearScreen: Stream[IO, Unit] = Stream.eval(putStrLn("")).repeat.take(50)

  def printRow(row: Seq[Boolean]): IO[Unit] = putStrLn(row.map(if (_) '\u25A0' else '.').mkString)
  def printRows(rows: Seq[Seq[Boolean]]): Stream[IO, Unit] = Stream(rows: _*).evalMap(printRow)

  def printNewRegion(region: Seq[Seq[Boolean]]): Stream[IO, Unit] =
    (clearScreen ++ printRows(region)).last.map(_ => ())

  def printEvery[A](duration: FiniteDuration)(print: Stream[IO, A])(implicit timer: Timer[IO]): IO[Unit] =
    Stream.awakeEvery[IO](duration)
      .zipWith(print)((_, i) => i)
      .compile.drain
}
