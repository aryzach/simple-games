package com.github.lpld.games.gameoflife

import com.github.lpld.games.gameoflife.GameOfLife._
import fs2.{Pure, Stream}
import scalaz.EphemeralStream

/**
  * @author leopold
  * @since 20/09/18
  */
object GameOfLife {

  type Size = Int
  type Pos = (Int, Int)

  case class Board(rows: Vector[Vector[Boolean]]) extends AnyVal {

    def sizeX: Size = rows.length
    def sizeY: Size = rows.headOption.map(_.length).getOrElse(0)

    def isCellAlive(pos: Pos): Boolean = rows(pos._1)(pos._2)
  }
}

class Game(initial: Board, closed: Boolean = false) {
  private val sizeX: Size = initial.sizeX
  private val sizeY: Size = initial.sizeY

  val boards: Stream[Pure, Board] = Stream.iterate(initial)(evolve)

  private def evolve(board: Board): Board = Board(
    board.rows.zipWithIndex.map { case (row, x) =>
      row.zipWithIndex.map { case (alive, y) =>
        val aliveNeighbors = countNeighbours(board, (x, y))
        aliveNeighbors == 3 || (aliveNeighbors == 2 && alive)
      }
    }
  )

  private def countNeighbours(board: Board, pos: Pos): Int = neighbIdxs(pos) count board.isCellAlive

  private def neighbIdxs(pos: Pos): Seq[Pos] = {
    def norm(v: Int, max: Int): Option[Int] =
      if (v < 0) { if (closed) Some(max - 1) else None }
      else if (v >= max) { if (closed) Some(0) else None }
      else Some(v)

    for {
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j != 0

      x <- norm(pos._1 + i, sizeX)
      y <- norm(pos._2 + j, sizeY)

    } yield (x, y)
  }
}

