package com.github.lpld.gameoflife

import com.github.lpld.gameoflife.GameOfLife.Board
import scalaz.Scalaz._
import scalaz.effect.IO.{putLn, putStrLn}
import scalaz.effect.{IO, SafeApp}

/**
  * @author leopold
  * @since 21/09/18
  */
object GameApp extends SafeApp {

  override def runc: IO[Unit] = {

    val game = new Game(Board(initialRows), closed = true)

    game.boardsStream.map(printBoard).sequence.map(_ => ())
  }

  val printEmptyLine: IO[Unit] = putStrLn("")
  val clearScreen: IO[Unit] = (1 to 30).toList.map(_ => printEmptyLine).sequence.map(_ => ())

  def sleep(millis: Int): IO[Unit] = IO { Thread.sleep(millis) }

  def printRows(board: Board): IO[Unit] = for (_ <- board.rows.map(r => putLn(r)).sequence) yield ()

  def printBoard(board: Board): IO[Unit] =
    for {
      _ <- clearScreen
      _ <- printRows(board)
      _ <- sleep(200)
    } yield ()

  val initialRows = Vector(
    row(".X..........................."),
    row("..X.........................."),
    row("XXX.........................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row("............................."),
    row(".............................")
  )

  def row(r: String) = Row(r.map(_ == 'X').toVector)
}
