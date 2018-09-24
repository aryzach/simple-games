package com.github.lpld.games.tetris

import org.scalatest.{FunSuite, Matchers}
import scalaz.Maybe.{Empty, Just}
import TetrisConsole._

/**
  * @author leopold
  * @since 21/09/18
  */
class RectRegionSuite extends FunSuite with Matchers {

  test("RectRegion.inject should inject a single cell into any coordinate of an empty region") {
    val size = 10
    val empty = RectRegion(size, size)
    val piece = RectRegion("X")

    def regionWithSingleCell(x: Int, y: Int) = RectRegion(
      for (i <- 0 until size) yield
        for (j <- 0 until size) yield i == x && j == y
    )

    for (x <- 0 until size)
      for (y <- 0 until size)
        empty.inject(piece, Coord(x, y)) shouldEqual Just(regionWithSingleCell(x, y))
  }

  test("RectRegion.inject should not inject cells outside of the region") {
    val empty = RectRegion(5, 5)
    val piece = RectRegion("X")

    empty.inject(piece, Coord(-1, 0)) shouldEqual Empty()
    empty.inject(piece, Coord(2, -1)) shouldEqual Empty()
    empty.inject(piece, Coord(5, 2)) shouldEqual Empty()
    empty.inject(piece, Coord(3, 5)) shouldEqual Empty()
  }

  test("RectRegion.inject should inject a piece if there is enough room") {
    val region = RectRegion(
      """....
        |....
        |.XX.
        |XXX.""")

    val piece = RectRegion(
      """XX
        |.X
        |.X""")

    val expected = Seq(
      Coord(0, 0) -> Empty(),
      Coord(0, 1) -> Empty(),

      Coord(0, 2) -> Just(RectRegion(
        """..XX
          |...X
          |.XXX
          |XXX.""")),

      Coord(3, 3) -> Empty(),

      Coord(1, 2) -> Just(RectRegion(
        """....
          |..XX
          |.XXX
          |XXXX"""))
    )

    expected.foreach { case (coord, result) =>
      region.inject(piece, coord) shouldEqual result
    }
  }

  test("RectRegion.rotate should rotate the region clockwise") {
    val pieces = Seq(
      """.X
        |XX
        |.X""",

      """.X.
        |XXX""",

      """X.
        |XX
        |X.""",

      """XXX
        |.X.""",

      """.X
        |XX
        |.X""",
    ).map(_.stripMargin)

    for ((prev, next) <- pieces.zip(pieces.tail)) {
      val rotated = RectRegion(prev).rotate

      rotated shouldEqual RectRegion(next)
      printRegion(rotated) shouldEqual next
    }
  }

  test("RectRegion.mirror should vertically mirror the piece") {
    val pieces = Seq(
      """X.
        |X.
        |XX""" ->
      """.X
        |.X
        |XX""",

      """XX
        |XX""" ->
      """XX
        |XX""",

      """X.
        |XX
        |.X""" ->
      """.X
        |XX
        |X."""
    ).map { case (s1, s2) => (s1.stripMargin, s2.stripMargin) }

    pieces.foreach { case (s1, s2) =>
      val piece1 = RectRegion(s1)
      val piece2 = RectRegion(s2)

      piece1.mirror shouldEqual piece2
      printRegion(piece1.mirror) shouldEqual s2

      piece2.mirror shouldEqual piece1
      printRegion(piece2.mirror) shouldEqual s1
    }
  }
}