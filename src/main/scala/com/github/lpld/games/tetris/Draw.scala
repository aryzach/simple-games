package com.github.lpld.games.tetris

import cats.instances.all._
import cats.syntax.all._
import cats.data.State
import cats.effect.IO
import com.github.lpld.games.tetris.Draw.DrawCommand
import org.fusesource.jansi.{Ansi, AnsiConsole}

/**
  * Based on https://github.com/m50d/console-game/blob/master/src/main/scala/example/Draw.scala
  *
  * @author leopold
  * @since 26/09/18
  */
object Draw {

  type DrawCommand = State[Ansi, Unit]

  private def ansi(modify: Ansi => Ansi): DrawCommand = State.modify[Ansi](modify)

  def eraseScreen: DrawCommand = ansi(_.eraseScreen())
  def goto(x: Int, y: Int): DrawCommand = ansi(_.cursor(x, y))
  def print(s: String): DrawCommand = ansi(_.a(s))

  def printAt(x: Int, y: Int, s: String): DrawCommand = goto(x, y) *> print(s)

  def printLinesAt(x: Int, y: Int, lines: Vector[String]): DrawCommand = {
    lines.zipWithIndex
      .traverse { case (line, idx) => printAt(x + idx, y, line) }
      .map(_ => Unit)
  }
}

object Console {

  def draw(a: DrawCommand) = IO { AnsiConsole.out.println(a.run(Ansi.ansi()).value._1) }
}

