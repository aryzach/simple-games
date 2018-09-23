package com.github.lpld.games.tetris

import scalaz.EphemeralStream, EphemeralStream._

/**
  * @author leopold
  * @since 21/09/18
  */
sealed trait Action
object Action {
  case object Start extends Action
  case class Fall(piece: RectRegion, coord: Coord) extends Action
}

case class State
(
  field: RectRegion,
  nextAction: Action,
  pieces: EphemeralStream[RectRegion]
)

object Tetris {

  type ShapeState
}

class Tetris(height: Int, width: Int) {

  private val start: Coord = Coord(0, width / 2)

  private val initial = State(RectRegion(height, width), Action.Start, Pieces.infiniteStream)

  val states: EphemeralStream[State] = EphemeralStream.iterate(initial)(evolve)

  private def evolve(state: State): State = {

    state match {
      case State(field, Action.Start, nextPiece ##:: rest) => ???
      case State(field, Action.Fall(shape, coord), pieces) => ???
    }

    ???
  }
}
