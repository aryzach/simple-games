package com.github.lpld.games.tetris

/**
  * @author leopold
  * @since 22/09/18
  */
case class Coord(x: Int, y: Int) {

  def rowDown = Coord(x + 1, y)

  def left = Coord(x, y - 1)
  def right = Coord(x, y + 1)
}
