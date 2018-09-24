package com.github.lpld.games.tetris

import fs2.{Pure, Stream}

object Pieces {
  val O = RectRegion(
    """XX
      |XX""")

  val I = RectRegion(
    """X
      |X
      |X
      |X""")

  val J = RectRegion(
    """X.
      |X.
      |XX""")

  val S = RectRegion(
    """.XX
      |XX.""")

  val T = RectRegion(
    """XXX
      |.X."""
  )

  val allPossiblePieces: Seq[RectRegion] = {
    def multF[A](f: A => A)(times: Int) = List.fill(times)(f)
      .foldLeft((a => a): A => A)(_ andThen _)

    for {
      m <- 0 to 1 // mirror
      r <- 0 to 3 // rotate
      p <- List(O, I, J, S, T)

      transform = multF[RectRegion](_.mirror)(m) andThen multF[RectRegion](_.rotate)(r)
    } yield transform(p)
  }

  // for testing. infinite, but not random stream of pieces
  val infiniteStream: Stream[Pure, RectRegion] = Stream(allPossiblePieces: _*).repeat

}
