package com.github.lpld.games.tetris

import cats.effect.{ContextShift, IO, Timer}
import com.github.lpld.games.tetris.Event._
import com.github.lpld.games.tetris.Tetris.{PiecesSource, downInterval}
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
  case object Down extends Move
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
case class GameState
(
  status: Status,
  field: RectRegion,
  fieldWithPiece: RectRegion,
  activePiece: Option[(RectRegion, Coord)],
  piecesSource: (RectRegion, PiecesSource),
  score: Int,
  level: Int,
  linesCleared: Int
)

object Tetris {

  type PiecesSource = Stream[Pure, RectRegion]
  val downInterval = 2
  val linesPerLevel = 10
}

class Tetris(height: Int, width: Int, interactions: Stream[IO, Move])
            (implicit timer: Timer[IO], contextShift: ContextShift[IO]) {

  private val start: Coord = Coord(0, width / 2 - 1)

  /**
    * This stream reflects all changes in the state of game field.
    */
  val gameSates: Stream[IO, GameState] = {
    val emptyField = RectRegion(height, width)

    // Two sources of events:
    // 1. Regular ticks
    val tick: Stream[IO, Event] = Stream.awakeEvery[IO](500.millis).map(_ => Tick)
    // 2. User's interactions
    val userMoves: Stream[IO, Event] = interactions.map(UserAction)
    // merge them
    val allEvents: Stream[IO, Event] = tick merge userMoves

    val initial = GameState(
      status = Status.Active,
      field = emptyField,
      fieldWithPiece = emptyField,
      activePiece = None,
      piecesSource = pullNextPiece(Pieces.infiniteStream),
      score = 0,
      level = 1,
      linesCleared = 0,
    )

    //
    //    val ticks = Stream.iterate[IO, Int](1)(_ + 1)
    //      .map(i => (Stream.awakeEvery[IO]((1000 - i * 300).millis).map(_ => Tick), i))
    //
    //    val states = ticks.scan(Stream(initial).lift[IO])((in, tick) => {
    //      val s = in.last.map(_.get).flatMap { inSt =>
    //        val level = inSt.level
    //        (tick._1 merge userMoves)
    //          .scan(inSt)(nextState)
    //          .takeWhile(_.level == level, takeFailure = true)
    //      }
    //      s
    //    }).flatten

    val states = allEvents.scan(initial)(nextState)
    states.takeWhile(_.status != Status.Over)
  }

  /**
    * Compute next game state given the previous state and an event.
    */
  private def nextState(state: GameState, event: Event): GameState =
    event match {
      case Tick => state.activePiece match {
        case None => activateNew(state)
        case Some((piece, at)) =>
          injectExisting(state, piece, at.rowDown).getOrElse(activateNew(state))
      }

      case UserAction(move) =>
        state.activePiece.flatMap { case (piece, at) =>
          move match {
            case Move.Left => injectExisting(state, piece, at.left)
            case Move.Right => injectExisting(state, piece, at.right)
            case Move.Rotate =>
              val (rotated, newCoord) = rotate(piece, at)
              injectExisting(state, rotated, newCoord)
            case Move.Down => moveDown(state, piece, at)
          }
        }.getOrElse(state)
    }

  private def moveDown(state: GameState, piece: RectRegion, at: Coord) = {
    // trying to shift the piece (downInterval + 1) rows down:
    val shifts = Stream.unfold(at) { prevCoord =>
      val newCoord = prevCoord.rowDown
      state.field
        .inject(piece, newCoord)
        .map(f => ((f, newCoord), newCoord))
    }.take(downInterval + 1).compile.toList

    if (shifts.length == downInterval + 1) {
      // if succeeded, then the piece is still active. shifting it `downInterval` rows down:
      val (f, newCoord) = shifts(downInterval - 1)
      Some(state.copy(fieldWithPiece = f, activePiece = Some((piece, newCoord))))
    } else {
      // otherwise, taking the last shift position and making the piece inactive:
      shifts.lastOption.map { case (f, _) =>
        state.copy(
          field = f,
          fieldWithPiece = f,
          activePiece = None
        )
      }
    }
  }
  private def activateNew(state: GameState) = checkFilledRows(state).getOrElse(injectNew(state))

  private def checkFilledRows(state: GameState): Option[GameState] = {
    state.fieldWithPiece.clearFilledRows
      .map { case (newField, clearedCount) =>
        val newScore = state.score + scoreFor(clearedCount)
        val linesCleared = state.linesCleared + clearedCount
        val nextLevel = linesCleared >= Tetris.linesPerLevel
        val level = if (nextLevel) state.level + 1 else state.level

        state.copy(
          field = newField,
          fieldWithPiece = newField,
          score = newScore,
          linesCleared = if (nextLevel) 0 else linesCleared,
          level = level
        )
      }
  }

  private def scoreFor(cleared: Int): Int = cleared match {
    case 1 => 100
    case 2 => 400
    case 3 => 800
    case 4 => 1200
  }

  private def rotate(piece: RectRegion, at: Coord): (RectRegion, Coord) = {
    val diff = (piece.height - piece.width) / 2
    (piece.rotate, Coord(at.x + diff, at.y - diff))
  }

  private def injectNew(state: GameState): GameState = {

    val newSource = pullNextPiece(state.piecesSource._2)

    tryInject(state.fieldWithPiece, state.piecesSource._1, start, newSource, state)
      .getOrElse(state.copy(
        status = Status.Over,
        field = state.fieldWithPiece,
        activePiece = None
      ))
  }

  private def injectExisting(state: GameState, piece: RectRegion, at: Coord) =
    tryInject(state.field, piece, at, state.piecesSource, state)

  private def tryInject(field: RectRegion, piece: RectRegion, at: Coord,
                        piecesSource: (RectRegion, PiecesSource),
                        state: GameState): Option[GameState] =
    field.inject(piece, at)
      .map(result => state.copy(
        field = field,
        fieldWithPiece = result,
        activePiece = Some(piece, at),
        piecesSource = piecesSource)
      )

  private def pullNextPiece(pieces: PiecesSource): (RectRegion, PiecesSource) =
    (pieces.head.toList.head, pieces.tail)
}
