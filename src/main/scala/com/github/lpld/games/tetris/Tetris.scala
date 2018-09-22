package com.github.lpld.games.tetris

import com.github.lpld.games.tetris.Tetris.{Board, Coord}
import scalaz.EphemeralStream

/**
  * @author leopold
  * @since 21/09/18
  */
object Tetris {

  type Coord = (Int, Int)
  type Board = Seq[Seq[Boolean]]
}

case class Shape(cells: Board) extends AnyVal {

  def height: Int = cells.length
  def width: Int = cells.headOption.map(_.length).getOrElse(0)

  def center: Coord = ???

  def rotate: Shape =
    if (height == 0) this
    else Shape {
      for (i <- 0 until width) yield
        for (j <- (0 until height).reverse) yield cells(j)(i)
    }
}

sealed trait Action
object Action {
  case object Start extends Action
  case class Fall(shape: Shape, coord: Coord) extends Action
}

case class State(board: Board, nextAction: Action)

class TetrisEngine(width: Int, height: Int) {

  val states = EphemeralStream.iterate(State(Seq.fill(height, width)(false), Action.Start))()

  private def evolve(state: State): State = {

  }
}
