package com.github.lpld.games.tetris

import scalaz.Scalaz._
import scalaz._

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

  private val allPossiblePieces: Seq[RectRegion] = {
    def multF[A](f: RectRegion => RectRegion)(times: Int) = Endo(f).multiply(times)

    for {
      m <- 0 to 1 // mirror
      r <- 0 to 3 // rotate
      p <- List(O, I, J, S, T)

      transform = multF(_.mirror)(m) andThen multF(_.rotate)(r)
    } yield transform(p)
  }

  // for testing. infinite, but not random stream of pieces
  val infiniteStream: EphemeralStream[RectRegion] = EphemeralStream
    .unfold(allPossiblePieces)(p => Some(p, p))
    .flatMap(p => EphemeralStream(p: _*))

}
