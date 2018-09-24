package com.github.lpld.games.tetris

import cats.effect.IO
import com.github.lpld.games.{ConsoleActions, SafeApp}

import scala.concurrent.duration.DurationInt

/**
  * @author leopold
  * @since 24/09/18
  */
object TetrisDemo extends SafeApp with ConsoleActions {

  def run: IO[Unit] = {
    val tetris = new Tetris(15, 20)

    printEvery(300.millis)(tetris.states.map(_.evolvingField.cells).flatMap(printNewRegion))
  }
}
