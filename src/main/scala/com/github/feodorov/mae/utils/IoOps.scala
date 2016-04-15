package com.github.feodorov.mae.utils

import java.util.logging.Logger

import scala.io.Source

/**
  * @author kfeodorov 
  * @since 15.04.16
  */
object IoOps {
  private[this] val logger = Logger.getLogger(getClass.getName)

  def withStream[A <: Source, B](in: => A) (f: A => B) {
    try {
      f(in)
    } finally {
      try {
        in.close()
      } catch {
        case e: Exception => logger.info(e.getMessage)
      }
    }
  }
}
