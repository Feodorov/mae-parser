package com.github.feodorov

import scala.util.Success
import com.github.feodorov.mae._
import MaeString._
import com.github.feodorov.mae.utils.IoOps

/**
  * @author kfeodorov 
  * @since 13.04.16
  */

object OpType extends Enumeration {
  type OpType = Value
  val read, write = Value
}

object Main extends App {
  if (args.length < 3) {
    println("Expected args: file.mae read/write key value")
    System.exit(1)
  }

  val opType = OpType.withName(args(1))
  val key = args(2)

  IoOps.withStream(scala.io.Source.fromFile(args.head)) { input =>
    new MaeParser(input.mkString).Mae.run() match {
      case Success(result: Seq[_]) =>
        val idx = result.indexWhere {
          case MaeObject("f_m_ct", _) => true
          case _ => false
        }

        if (idx == -1) {
          println("no f_m_ct block found")
          System.exit(1)
        }
        else {
          val fmct = result(idx).asInstanceOf[MaeObject]

          MaePrinter.print(System.out, if (opType == OpType.write) {
            val value = args(3)
            result.updated(idx, fmct.copy(fields = fmct.fields + (MaeString(key) -> MaeString(value))))
          } else if (opType == OpType.read) {
            Seq(fmct.fields(key))
          } else result)
        }
      case _ =>
        println("Cannot parse")
        System.exit(1)
    }
  }
}
