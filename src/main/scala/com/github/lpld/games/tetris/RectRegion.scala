package com.github.lpld.games.tetris

import scalaz.Maybe.{Empty, Just}
import scalaz.Scalaz._
import scalaz._

/**
  * Class that represents a rectangular region. Each of the cells of the region
  * can be in two states (true/false).
  *
  * @author leopold
  * @since 22/09/18
  */
case class RectRegion(cells: Seq[Seq[Boolean]]) extends AnyVal {

  def height: Int = cells.length
  def width: Int = cells.headOption.map(_.length).getOrElse(0)

  def apply(i: Int, j: Int): Boolean = cells(i)(j)

  /**
    * Rotate clockwise:
    * {{{
    *   .XX.    ...
    *   .X.. -> XXX
    *   .X..    ..X
    *           ...
    * }}}
    */
  def rotate: RectRegion =
    if (height == 0) this
    else RectRegion {
      for (i <- 0 until width) yield
        for (j <- (0 until height).reverse) yield this (j, i)
    }

  /**
    * Mirror vertically:
    * {{{
    *   .XX.    .XX.
    *   .X.. -> ..X.
    *   .X..    ..X.
    * }}}
    */
  def mirror: RectRegion =
    if (height == 0) this
    else RectRegion {
      for (i <- 0 until height) yield
        for (j <- (0 until width).reverse) yield this (i, j)
    }

  /**
    * Try "inject" a new {{{injectee}}} region into this region at coordinates {{{coord}}}.
    * "Injection" means that the resulting region will be the same size as "this" region and
    * that the corresponding cells of both regions will be combined (if possible). Two cells can be
    * combined if at least one of them is empty (false). If both cells are non-empty (true),
    * it means that there's no room for {{{injectee}}} region. In this case this function will
    * return [[scalaz.Maybe.Empty]]. Otherwise it will return new [[RectRegion]] that
    * is the result of the injection.
    */
  def inject(injectee: RectRegion, coord: Coord): Maybe[RectRegion] =

  // Check the boundaries. If we try to inject a region outside of
  // "this" region, we will return Empty
    if (coord.x < 0 || coord.y < 0 ||
        coord.x + injectee.height > this.height ||
        coord.y + injectee.width > this.width) Empty()
    else {
      // Combine two values only if they are not both true.
      def combineValues(v1: Boolean, v2: Boolean): Maybe[Boolean] =
        if (v1 && v2) Empty()
        else Just(v1 || v2)

      // Combine cells of this RectRegion at coordinates (x, y) with
      // corresponding cells of the injectee region.
      def combineCellsAt(x: Int, y: Int): Maybe[Boolean] =
        if (x < coord.x || y < coord.y ||
            x >= coord.x + injectee.height ||
            y >= coord.y + injectee.width) Just(this (x, y))
        else combineValues(this (x, y), injectee(x - coord.x, y - coord.y))

      // Combine all the cells. If some of the cells could not be combined, they will equal to Empty
      val combined =
        for (x <- (0 until height).toStream) yield
          for (y <- (0 until width).toStream) yield {
            combineCellsAt(x, y)
          }

//      combined.foldLeft(Maybe.just(Seq.empty[Seq[Boolean]]))((acc, row) =>
//        acc.flatMap(rows => row.sequence.map(r => rows :+ r)))

      // Turn Seq[Seq[Maybe[Boolean]] into Maybe[Seq[Seq[Boolean]]. If any of the cells is Empty,
      // then the whole regions will be empty
      combined
        .map(_.sequence).sequence
        .map(RectRegion(_))
    }

}

object RectRegion {

  def apply(height: Int, width: Int): RectRegion = RectRegion(Seq.fill(height, width)(false))

  def apply(str: String): RectRegion = {
    val cells = for (line <- str.stripMargin.lines) yield {
      for (cell <- line) yield cell == 'X'
    }
    RectRegion(cells.toSeq)
  }
}
