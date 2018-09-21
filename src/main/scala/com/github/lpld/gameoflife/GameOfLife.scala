package com.github.lpld.gameoflife

import com.github.lpld.gameoflife.GameOfLife._
import scalaz.Show

/**
  * @author leopold
  * @since 20/09/18
  */
object GameOfLife {

  type Size = Int
  type Pos = (Int, Int)

  case class Board(rows: Vector[Row]) extends AnyVal {

    def sizeX: Size = rows.length
    def sizeY: Size = if (sizeX == 0) 0 else rows(0).size

    def isCellAlive(pos: Pos): Boolean = rows(pos._1).isCellAlive(pos._2)
  }
}

case class Row(cells: Vector[Boolean]) extends AnyVal {

  def size: Size = cells.length
  def isCellAlive(coord: Int): Boolean = cells(coord)
}

object Row {

  implicit val showRow: Show[Row] = Show.shows(_.cells.map(if (_) 'X' else '.').mkString)
}

class Game(initial: Board, closed: Boolean = false) {

  val sizeX: Size = initial.sizeX
  val sizeY: Size = initial.sizeY

  val boardsStream: Stream[Board] = Stream.iterate(initial)(evolve)

  def evolve(board: Board): Board = Board(
    board.rows.zipWithIndex.map { case (row, x) =>
      Row(
        row.cells.zipWithIndex.map { case (alive, y) =>
          val aliveNeighbors = countNeighbours(board, (x, y))
          aliveNeighbors == 3 || (aliveNeighbors == 2 && alive)
        }
      )
    }
  )

  def countNeighbours(board: Board, pos: Pos): Int = neighbIdxs(pos) count board.isCellAlive

  def neighbIdxs(pos: Pos): Seq[Pos] = {
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

