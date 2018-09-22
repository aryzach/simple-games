package com.github.lpld.games.tetris

/**
  * @author leopold
  * @since 22/09/18
  */
case class Piece(cells: RectRegion) extends AnyVal {

  def height: Int = cells.length
  def width: Int = cells.headOption.map(_.length).getOrElse(0)

  def center: Coord = ???

  def rotate: Piece =
    if (height == 0) this
    else Piece {
      for (i <- 0 until width) yield
        for (j <- (0 until height).reverse) yield cells(j)(i)
    }

  def mirror: Piece =
    if (height == 0) this
    else Piece {
      for (i <- 0 until height) yield
        for (j <- (0 until width).reverse) yield cells(i)(j)
    }
}

object Piece {

  val O = Piece(
    """XX
      |XX""")

  val I = Piece(
    """X
      |X
      |X
      |X""")

  val J = Piece(
    """X.
      |X.
      |XX""")

  val S = Piece(
    """.XX
      |XX.""")

  val T = Piece(
    """XXX
      |.X."""
  )

  def apply(str: String): Piece = {
    val cells = for (line <- str.stripMargin.lines) yield {
      for (cell <- line) yield cell == 'X'
    }
    Piece(cells.toSeq)
  }

}