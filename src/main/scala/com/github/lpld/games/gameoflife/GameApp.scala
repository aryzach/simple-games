package com.github.lpld.games.gameoflife

import cats.effect.IO
import com.github.lpld.games.{ConsoleActions, SafeApp}
import com.github.lpld.games.gameoflife.GameOfLife.Board

import scala.concurrent.duration.DurationInt

/**
  * @author leopold
  * @since 21/09/18
  */
object GameApp extends SafeApp with ConsoleActions {

  def run: IO[Unit] = {
    val game = new Game(Board(initialRows), closed = true)

    printEvery(200.millis)(game.boards.flatMap(b => printNewRegion(b.rows)))
  }

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
