package com.github.lpld.games.gameoflife

import com.github.lpld.games.IOExt._
import com.github.lpld.games.gameoflife.GameOfLife.Board
import scalaz.Scalaz._
import scalaz.effect.IO.putStrLn
import scalaz.effect.{IO, SafeApp}

/**
  * @author leopold
  * @since 21/09/18
  */
object GameApp extends SafeApp {

  override def runc: IO[Unit] = {
    val game = new Game(Board(initialRows), closed = true)
    game.boards.mapSeqUnit(printBoard)
  }

  val printEmptyLine: IO[Unit] = putStrLn("")
  val clearScreen: IO[Unit] = (1 to 30).toList.mapSeqUnit(_ => printEmptyLine)
  val sleep: IO[Unit] = IO { Thread.sleep(200) }

  def printRow(row: Vector[Boolean]): IO[Unit] =
    putStrLn(row.map(if (_) '\u25A0' else '.').mkString)

  def printRows(board: Board): IO[Unit] = board.rows.mapSeqUnit(printRow)

  def printBoard(board: Board): IO[Unit] =
    for {
      _ <- clearScreen
      _ <- printRows(board)
      _ <- sleep
    } yield ()

  val initialRows = Vector(
    row("............................................"),
    row("............................................"),
    row("............................................"),
    row("............................................"),
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

  def row(r: String) = r.map(_ == 'X').toVector
}
