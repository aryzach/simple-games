package com.github.lpld.games.tetris

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream

/**
 * @author leopold
 * @since 24/09/18
 */
object TetrisApp extends IOApp {

  val height = 30 
  val width = 30 

  override def run(args: List[String]): IO[ExitCode] = {
    val tetris = new Tetris(height, width, UserInput.moves)

    Console.draw(Draw.eraseScreen) *>
    tetris.gameSates.flatMap(r => Stream.eval(printState(r))).compile.drain *>
    IO.pure(ExitCode.Success)
  }
  private def printState(state: GameState): IO[Unit] =
    Console.draw(
      Draw.printLinesAt(1, 1, state.fieldWithPiece.cells.map(showRow)) *>

      Draw.printAt(2, width + 2, s"Score: ${state.score}") *>

      Draw.printAt(3, width + 2, s"Level: ${state.level}") *>

      Draw.printAt(5, width + 2, "Next: ") *>

      Draw.printLinesAt(7, width + 3, Vector.fill(4)("....")) *> // erase
      Draw.printLinesAt(7, width + 3, state.piecesSource._1.cells.map(showRow)) *>

      // intentionally adding extra space in the end to clear previous output:
      Draw.printAt(12, width + 2, s"Lines left: ${Tetris.linesPerLevel - state.linesCleared} ") *>

      Draw.goto(height + 1, 0)
    )   

  private def showRow(row: Seq[Boolean]) = row.map(if (_) '\u25A0' else '.').mkString

}
