package com.github.lpld.games.tetris

import com.github.lpld.games.tetris.Action.GameOver
import fs2.{Pure, Stream}

/**
  * Next action that should be performed.
  */
sealed trait Action
object Action {
  /**
    * Pick a new piece and put it into the field.
    */
  case object NewPiece extends Action
  /**
    * Place the [[piece]] at specific coordinate [[at]]. [[firstStep]] indicates if the piece
    * has just been put to the field.
    */
  case class PlacePiece(piece: RectRegion, at: Coord, firstStep: Boolean) extends Action
  /**
    * You lost!
    */
  case object GameOver extends Action
}

/**
  * State of the game.
  *
  * @param nextAction    Next action
  * @param field         Game field
  * @param evolvingField Game field plus currently falling piece. When current piece reaches the
  *                      lowest possible position, [[evolvingField]] will become new [[field]]
  * @param pieces        Infinite source of pieces
  */
case class State
(
  nextAction: Action,
  field: RectRegion,
  evolvingField: RectRegion,
  pieces: Stream[Pure, RectRegion]
)

object Tetris {

  type ShapeState
}

class Tetris(height: Int, width: Int) {

  private val start: Coord = Coord(0, width / 2)

  private val emptyField = RectRegion(height, width)
  private val initial = State(Action.NewPiece, emptyField, emptyField, Pieces.infiniteStream)

  val states: Stream[Pure, State] = Stream.iterate(initial)(evolve)

  private def evolve(state: State): State = {

    state match {
      // New piece:
      case State(Action.NewPiece, field, evolvingField, pieces) =>
        // taking new piece:
        val newPiece = pieces.head.toList.head

        // and start moving it:
        State(
          Action.PlacePiece(newPiece, start, firstStep = true),
          field, evolvingField, pieces.tail
        )

      // A piece is falling
      case State(Action.PlacePiece(piece, at, firstStep), field, evolvingField, pieces) =>

        // Trying to place the piece inside the field at a given coordinate
        field.inject(piece, at) match {
          case None =>
            // If not successful, then either end the game
            if (firstStep) State(GameOver, evolvingField, evolvingField, pieces)
            // or start new piece:
            else State(Action.NewPiece, evolvingField, evolvingField, pieces)
          case Some(newField) =>
            // If successful, then trying to move it further:
            State(
              Action.PlacePiece(piece, at.rowBelow, firstStep = false),
              field, newField, pieces
            )
        }

      // todo: stop running
      case State(Action.GameOver, _, _, _) => ???
    }
  }
}
