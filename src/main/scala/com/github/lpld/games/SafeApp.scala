package com.github.lpld.games

import cats.effect.IO

/**
  * @author leopold
  * @since 24/09/18
  */
trait SafeApp {

  def run: IO[Unit]

  final def main(args: Array[String]): Unit = run.unsafeRunSync()
}
