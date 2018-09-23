package com.github.lpld.games.gameoflife

import java.io.IOException

import com.github.lpld.games.gameoflife.GameOfLife.Board
import scalaz.Scalaz._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import scalaz.zio.{App, IO, Schedule}

import scala.concurrent.duration.DurationInt

/**
  * @author leopold
  * @since 21/09/18
  */
object GameApp extends App {

  type Res[T] = IO[IOException, T]

  private def printError(err: IOException): IO[Nothing, Unit] =
    putStrLn(err.getMessage).catchAll(_ => IO.unit)

  override def run(args: List[String]): IO[Nothing, ExitStatus] = {
    val game = new Game(Board(initialRows), closed = true)

    game.boards
      .traverse(printBoard)
      .catchAll(printError) *> IO.point(ExitStatus.ExitNow(0))
  }

  val printEmptyLine: Res[Unit] = putStrLn("")
  val clearScreen: Res[Int] = printEmptyLine.repeat(Schedule.recurs(30))

  def printRow(row: Vector[Boolean]): Res[Unit] =
    putStrLn(row.map(if (_) '\u25A0' else '.').mkString)

  def printRows(board: Board): Res[Vector[Unit]] = board.rows.traverse(printRow)

  def printBoard(board: Board): Res[Unit] =
    clearScreen *> printRows(board) *> IO.sleep(150.millis)

  val initialRows = Vector(
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
