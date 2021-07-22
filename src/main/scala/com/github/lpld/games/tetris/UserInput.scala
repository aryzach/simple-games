package com.github.lpld.games.tetris

import cats.effect.IO
import fs2.Stream
import org.jline.terminal.{Terminal, TerminalBuilder}

/**
  * @author leopold
  * @since 26/09/18
  */
object UserInput {

  private val reader = createTerminal.reader()
  private val inputs = Stream.repeatEval(IO { reader.read() })

  val moves: Stream[IO, Move] = inputs.collect {
    case 'w' => Move.Rotate
    case 'd' => Move.Right
    case 'a' => Move.Left
    case 's' => Move.Down
    case ' ' => Move.FullDown
  }

  private def createTerminal: Terminal = {
    val t = TerminalBuilder.builder()
      .jansi(true)
      .system(true)
      .build()

    t.enterRawMode()
    t
  }
}
