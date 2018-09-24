package com.github.lpld.games.tetris

import cats.effect.{ContextShift, IO, Timer}
import com.github.lpld.games.tetris.Action._
import com.github.lpld.games.tetris.Tetris.PiecesSource
import fs2.{Pure, Stream}

import scala.concurrent.duration.DurationLong

/**
  * Next action that should be performed.
  */
sealed trait Action
object Action {
  case object Tick extends Action
  case class UserAction(move: Move) extends Action
}

sealed trait Move
object Move {
  case object Rotate extends Move
  case object Right extends Move
  case object Left extends Move
  //  case object Down extends Move
}

case class State
(
  gameActive: Boolean,
  field: RectRegion,
  fieldWithPiece: RectRegion,
  activePiece: Option[(RectRegion, Coord)],
  piecesSource: Stream[Pure, RectRegion]
)

object Tetris {

  type PiecesSource = Stream[Pure, RectRegion]
}

class Tetris(height: Int, width: Int, interactions: Stream[IO, Move])
            (implicit timer: Timer[IO], contextShift: ContextShift[IO]) {

  private val start: Coord = Coord(0, width / 2 - 1)

  val states: Stream[IO, RectRegion] = {
    val emptyField = RectRegion(height, width)

    val tick: Stream[IO, Action] = Stream.awakeEvery[IO](800.millis).map(_ => Tick)
    val userMoves: Stream[IO, Action] = interactions.map(UserAction)

    val allActions: Stream[IO, Action] = tick.merge(userMoves)

    val initial = State(gameActive = true, emptyField, emptyField, None, Pieces.infiniteStream)

    allActions
      .scan(initial)(nextState)
      .takeWhile(_.gameActive)
      .map(_.fieldWithPiece)
  }

  private def nextState(state: State, action: Action): State =
    action match {
      case Tick => state.activePiece match {
        case None => injectNew(state)
        case Some((piece, at)) =>
          inject(state.field, piece, at.rowDown, state.piecesSource)
            .getOrElse(injectNew(state))
      }

      case UserAction(move) =>
        state.activePiece.flatMap { case (piece, at) =>
          move match {
            case Move.Left => inject(state.field, piece, at.left, state.piecesSource)
            case Move.Right => inject(state.field, piece, at.right, state.piecesSource)
            case Move.Rotate =>
              val (rotated, newCoord) = rotate(piece, at)
              inject(state.field, rotated, newCoord, state.piecesSource)
          }
                                  }.getOrElse(state)
    }

  private def rotate(piece: RectRegion, at: Coord): (RectRegion, Coord) = {
    val diff = (piece.height - piece.width) / 2
    (piece.rotate, Coord(at.x + diff, at.y - diff))
  }

  private def injectNew(currentState: State): State = {

    val (newPiece, newSource) = getNextPiece(currentState.piecesSource)

    inject(currentState.fieldWithPiece, newPiece, start, newSource)
      .getOrElse(State(
        gameActive = false, currentState.fieldWithPiece, currentState
          .fieldWithPiece, None, newSource
      ))
  }

  private def inject(field: RectRegion, piece: RectRegion, at: Coord,
                     piecesSource: PiecesSource): Option[State] =
    field.inject(piece, at)
      .map(result => State(gameActive = true, field, result, Some(piece, at), piecesSource))


  private def getNextPiece(pieces: PiecesSource): (RectRegion, PiecesSource) =
    (pieces.head.toList.head, pieces.tail)
}
