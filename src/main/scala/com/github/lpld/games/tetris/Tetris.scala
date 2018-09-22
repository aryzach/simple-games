package com.github.lpld.games.tetris

import com.github.lpld.games.tetris.Tetris.{Board, Coord}
import scalaz.EphemeralStream

/**
  * @author leopold
  * @since 21/09/18
  */
sealed trait Action
object Action {
  case object Start extends Action
  case class Fall(shape: Piece, coord: Coord) extends Action
}

case class State(board: RectRegion, nextAction: Action)

class Tetris(width: Int, height: Int) {

  private val initial = State(Seq.fill(height, width)(false), Action.Start)

  val states: EphemeralStream[State] = EphemeralStream.iterate(initial)(evolve)

  private def evolve(state: State): State = {

    state match {
      case State(board, Action.Start) => ???
      case State(board, Action.Fall(shape, coord)) => ???
    }

    ???
  }
}
