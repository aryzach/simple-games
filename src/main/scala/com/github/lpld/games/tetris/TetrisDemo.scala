package com.github.lpld.games.tetris

import cats.effect.{ExitCode, IO, IOApp}
import com.github.lpld.games.ConsoleActions
import fs2.Stream
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

/**
  * @author leopold
  * @since 24/09/18
  */
object TetrisDemo extends IOApp with ConsoleActions {

  //  def run: IO[Unit] = {
  //    val tetris = new Tetris(15, 20)
  //
  //    printEvery(300.millis)(tetris.states.map(_.evolvingField.cells).flatMap(printNewRegion))
  //  }
  override def run(args: List[String]): IO[ExitCode] = {

    val userMoves = Stream.awakeEvery[IO](300.millis).map(_ => Move.Rotate)

    val tetris = new Tetris(15, 20, userMoves)

    tetris.states.flatMap(r => printNewRegion(r.cells)).compile.drain *>
    IO.pure(ExitCode.Success)
  }
}
