package com.github.lpld.games.tetris

import org.scalatest.{FunSuite, Matchers}

/**
  * @author leopold
  * @since 21/09/18
  */
class PieceSuite extends FunSuite with Matchers {

  test("Piece.rotate should rotate the piece clockwise") {
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
      val rotated = Piece(prev).rotate

      rotated shouldEqual Piece(next)
      printPiece(rotated) shouldEqual next
    }
  }

  test("Piece.mirror should vertically mirror the piece") {
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
      val piece1 = Piece(s1)
      val piece2 = Piece(s2)

      piece1.mirror shouldEqual piece2
      printPiece(piece1.mirror) shouldEqual s2

      piece2.mirror shouldEqual piece1
      printPiece(piece2.mirror) shouldEqual s1
    }
  }

  def printPiece(piece: Piece): String =
    piece.cells.map(
      row => row.map(if (_) 'X' else '.').mkString
    ).mkString("\n")

}
