package com.github.lpld.games.tetris

import org.scalatest.{FunSuite, Matchers}

/**
  * @author leopold
  * @since 21/09/18
  */
class ShapeSuite extends FunSuite with Matchers {

  test("Shape.rotate should move the shape clockwise") {
    val shapes = Seq(
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

    for ((prev, next) <- shapes.zip(shapes.tail)) {
      val rotated = parseShape(prev).rotate
      rotated.cells shouldEqual parseShape(next).cells
      printShape(rotated) shouldEqual next
    }
  }

  def parseShape(str: String): Shape = {
    val cells = for (line <- str.lines) yield {
      for (cell <- line) yield cell == 'X'
    }
    Shape(cells.toSeq)
  }

  def printShape(shape: Shape): String =
    shape.cells.map(
      row => row.map(if (_) 'X' else '.').mkString
    ).mkString("\n")

}
