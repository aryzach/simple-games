package com.github.lpld.games.tetris

import cats.instances.all._
import cats.syntax.all._
import cats.data.State
import cats.effect.IO
import org.fusesource.jansi.{Ansi, AnsiConsole}

/**
  * Based on https://github.com/m50d/console-game/blob/master/src/main/scala/example/Draw.scala
  *
  * @author leopold
  * @since 26/09/18
  */
object Draw {

  type AnsiState = State[Ansi, Unit]

  private def ansi(modify: Ansi => Ansi): AnsiState = State.modify[Ansi](modify)

  def eraseScreen: AnsiState = ansi(_.eraseScreen())
  def goto(x: Int, y: Int): AnsiState = ansi(_.cursor(x, y))
  def print(s: String): AnsiState = ansi(_.a(s))
  def println(s: String): AnsiState = print(s) *> ansi(_.newline())
  def printlns(s: Vector[String]): AnsiState = s traverse println map (_ => ())

  def materialize(a: AnsiState): Ansi = a.run(Ansi.ansi()).value._1

  def run(a: Ansi) = IO { AnsiConsole.out.println(a) }
}

