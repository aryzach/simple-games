package com.github.lpld.games

import scalaz._
import Scalaz._

/**
  * @author leopold
  * @since 22/09/18
  */
object IOExt {

  implicit class traverseOps[A, T[_] : Traverse](t: T[A]) {

    def mapSeqUnit[F[_] : Applicative, B](f: A => F[B]): F[Unit] = t.map(f).sequence.map(_ => ())
  }

}
