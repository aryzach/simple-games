package com.github.lpld.games.tetris

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import com.github.lpld.games.ConsoleActions
import fs2.Stream

/**
  * @author leopold
  * @since 24/09/18
  */
object TetrisDemo extends IOApp with ConsoleActions {

  override def run(args: List[String]): IO[ExitCode] = {
    val tetris = new Tetris(15, 20, UserInput.moves)

    Console.print(Draw.eraseScreen) *>
    tetris.fieldsStream.flatMap(r => Stream.eval(printField(r))).compile.drain *>
    IO.pure(ExitCode.Success)
  }

  private def printField(reg: RectRegion): IO[Unit] =
    Console.print(
      Draw.goto(1, 1) *>
      Draw.printlns(reg.cells.map(showRow))
    )

  private def showRow(row: Seq[Boolean]) = row.map(if (_) '\u25A0' else '.').mkString
}
