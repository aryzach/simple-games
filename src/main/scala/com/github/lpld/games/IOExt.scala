package com.github.lpld.games

import scalaz.Traverse
import scalaz.effect.IO
import scalaz.Scalaz._

/**
  * @author leopold
  * @since 22/09/18
  */
object IOExt {
  implicit class traverseOps[A, T[_]: Traverse](t: T[IO[A]]) {

    def flattenIO: IO[Unit] = t.sequence.map(_ => ())
  }
}
