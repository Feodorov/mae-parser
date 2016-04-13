package com.github.feodorov

import java.io.ByteArrayOutputStream

import scala.util.Success
import com.github.feodorov.mae._

/**
  * @author kfeodorov 
  * @since 13.04.16
  */
object Main extends App {
  if (args.length < 3) {
    println("Expected args: file.mae key value")
    System.exit(1)
  }

  //catch block is left intentionally
  val input = scala.io.Source.fromFile(args.head)
  val parser = new MaeParser(input.mkString)

  parser.Mae.run() match {
    case Success(result) =>
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
        val resultMae = result.updated(idx, fmct.copy(fields = fmct.fields + (MaeString(args(1)) -> MaeString(args(2)))))
        val baos = new ByteArrayOutputStream()
        MaePrinter.print(baos, resultMae)
        baos.close()
        println(baos.toString)
      }
    case _ =>
      println("Cannot parse")
      System.exit(1)
  }

  input.close()
}
