package com.github.lpld.games.tetris

import cats.instances.all._
import cats.syntax.all._

/**
 * Class that represents a rectangular region. Each of the cells of the region
 * can be in two states (true/false).
 *
 * @author leopold
 * @since 22/09/18
 */
case class RectRegion(cells: Vector[Vector[Boolean]]) extends AnyVal {

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
       * Remove filled rows and add empty rows on top of the region instead of them.
       */
      def clearFilledRows: Option[(RectRegion, Int)] = {

        val newField = cells.filterNot(_.forall(b => b))

        if (newField.length == this.height) None
        else {
          val cleared = this.height - newField.length
          Some(
            RectRegion(Vector.fill(cleared)(Vector.fill(this.width)(false)) ++ newField),
            cleared
          )
        }
      }

      /**
       * Try "inject" a new {{{injectee}}} region into this region at coordinates {{{coord}}}.
       * "Injection" means that the resulting region will be the same size as "this" region and
       * that the corresponding cells of both regions will be combined (if possible). Two cells can be
       * combined if at least one of them is empty (false). If both cells are non-empty (true),
       * it means that there's no room for {{{injectee}}} region. In this case this function will
       * return [[None]]. Otherwise it will return new [[RectRegion]] that
       * is the result of the injection.
       */
      def inject(injectee: RectRegion, coord: Coord): Option[RectRegion] =

        // Check the boundaries. If we try to inject a region outside of
      // "this" region, we will return Empty
      if (coord.x < 0 || coord.y < 0 ||
        coord.x + injectee.height > this.height ||
        coord.y + injectee.width > this.width) None
    else {
      // Combine two values only if they are not both true.
      def combineValues(v1: Boolean, v2: Boolean): Option[Boolean] =
        if (v1 && v2) None
        else Some(v1 || v2)

        // Combine cells of this RectRegion at coordinates (x, y) with
        // corresponding cells of the injectee region.
        def combineCellsAt(x: Int, y: Int): Option[Boolean] =
          if (x < coord.x || y < coord.y ||
            x >= coord.x + injectee.height ||
            y >= coord.y + injectee.width) Some(this (x, y))
        else combineValues(this (x, y), injectee(x - coord.x, y - coord.y))

        // Combine all the cells. If some of the cells could not be combined, they will equal to Empty
        val combined =
          for (x <- (0 until height).toStream) yield
        for (y <- (0 until width).toStream) yield {
          combineCellsAt(x, y)
        }

        //      combined.foldLeft(Some(Seq.empty[Seq[Boolean]]))((acc, row) =>
        //        acc.flatMap(rows => row.sequence.map(r => rows :+ r)))

        // Turn Seq[Seq[Option[Boolean]] into Option[Seq[Seq[Boolean]]. If any of the cells is Empty,
        // then the whole regions will be empty
        combined
          .map(_.sequence).sequence
          .map(RectRegion(_))
    }

}

object RectRegion {
  def apply(cells: Seq[Seq[Boolean]]): RectRegion = RectRegion(cells.map(_.toVector).toVector)

  def apply(height: Int, width: Int): RectRegion = RectRegion(Vector.fill(height, width)(false))

  def apply(str: String): RectRegion = {
    val cells = 
      for {
        line: String <- lines(str.stripMargin)
      } 
    yield {
      for {
        cell <- line
      }yield cell == 'X'
    }
    RectRegion(cells.toIndexedSeq)
  }

  // original lines would call the java lines function, giving a Stream instead of an iterator
  // linesIterator method depreciated w this Scala version?, supposedly this is fixed in a later Scala version
  def lines(s: String): Seq[String] =
    s.split("\n")
      .toSeq
      .map(_.trim)
      .filter(_ != "")

}


