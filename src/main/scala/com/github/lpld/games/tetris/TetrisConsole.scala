package com.github.lpld.games.tetris

/**
  * @author leopold
  * @since 24/09/18
  */
object TetrisConsole {

  def printRegion(piece: RectRegion): String =
    piece.cells.map(
      row => row.map(if (_) 'X' else '.').mkString
    ).mkString("\n")
}
