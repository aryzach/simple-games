package com.github.lpld.games.gameoflife

import fs2.Stream
import cats.effect.{IO, Timer}
import com.github.lpld.games.gameoflife.GameOfLife.Board

import scala.concurrent.ExecutionContext

import scala.concurrent.duration.DurationInt

/**
  * @author leopold
  * @since 21/09/18
  */
object GameApp extends App {

  val clearScreen = Stream.eval(putStrLn("")).repeat.take(30)

  val game = new Game(Board(initialRows), closed = true)

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  Stream.awakeEvery[IO](200.millis)
    .zipWith(game.boards.flatMap(printBoard))((_, i) => i)
    .compile.drain
    .unsafeRunSync()

  def putStrLn(s: String) = IO { println(s) }

  def printRow(row: Vector[Boolean]) =
    putStrLn(row.map(if (_) '\u25A0' else '.').mkString)

  def printRows(board: Board) = Stream(board.rows: _*).evalMap(printRow)

  def printBoard(board: Board) = (clearScreen ++ printRows(board)).last

  def initialRows = Vector(
    row("............................................"),
    row("...X........................................"),
    row("..X........................................."),
    row("..XXX......................................."),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("...................X...X...................."),
    row("....................X.X....................."),
    row(".....................X......................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................")
  )

  private def row(r: String) = r.map(_ == 'X').toVector
}
