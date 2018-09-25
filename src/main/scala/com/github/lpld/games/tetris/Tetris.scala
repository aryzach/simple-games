package com.github.lpld.games.tetris

import cats.effect.{ContextShift, IO, Timer}
import com.github.lpld.games.tetris.Event._
import com.github.lpld.games.tetris.Tetris.PiecesSource
import fs2.{Pure, Stream}

import scala.concurrent.duration.DurationLong

sealed trait Event
object Event {
  case object Tick extends Event
  case class UserAction(move: Move) extends Event
}

/**
  * Command for moving active piece
  */
sealed trait Move
object Move {
  case object Rotate extends Move
  case object Right extends Move
  case object Left extends Move
  //  case object Down extends Move
}

sealed trait Status
object Status {
  case object Active extends Status
  case object Paused extends Status
  case object Over extends Status
}

/**
  * @param status         Active, paused or over
  * @param field          Current state of the field (without the piece)
  * @param fieldWithPiece Current state of the field with active piece. When the piece reaches
  *                       the lowest possible location, [[fieldWithPiece]] will become new [[field]]
  * @param activePiece    Active piece along with its coordinates
  * @param piecesSource   Stream of pieces
  */
case class State
(
  status: Status,
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

  /**
    * This stream reflects all changes in the state of game field.
    */
  val fieldsStream: Stream[IO, RectRegion] = {
    val emptyField = RectRegion(height, width)

    // Two sources of events:
    // 1. Regular ticks
    val tick: Stream[IO, Event] = Stream.awakeEvery[IO](800.millis).map(_ => Tick)
    // 2. User's interactions
    val userMoves: Stream[IO, Event] = interactions.map(UserAction)

    // merge them
    val allEvents: Stream[IO, Event] = tick merge userMoves

    val initial = State(Status.Active, emptyField, emptyField, None, Pieces.infiniteStream)

    allEvents
      .scan(initial)(nextState)
      .takeWhile(_.status != Status.Over)
      .map(_.fieldWithPiece)
  }

  /**
    * Compute next game state given the previous state and an event.
    */
  private def nextState(state: State, event: Event): State =
    event match {
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
        Status.Over, currentState.fieldWithPiece, currentState.fieldWithPiece, None, newSource
      ))
  }

  private def inject(field: RectRegion, piece: RectRegion, at: Coord,
                     piecesSource: PiecesSource): Option[State] =
    field.inject(piece, at)
      .map(result => State(Status.Active, field, result, Some(piece, at), piecesSource))

  private def getNextPiece(pieces: PiecesSource): (RectRegion, PiecesSource) =
    (pieces.head.toList.head, pieces.tail)
}
