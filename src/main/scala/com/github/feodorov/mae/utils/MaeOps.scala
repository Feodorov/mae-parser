package com.github.feodorov.mae.utils

import java.io.{InputStream, OutputStream}

import com.github.feodorov.mae._

import scala.util.{Failure, Success}


/**
  * @author kfeodorov 
  * @since 15.04.16
  */
object MaeOps {

  def put(in: InputStream, out: OutputStream, objName: String, key: String, value: String): Unit =
    IoOps.withStream(scala.io.Source.fromInputStream(in)) { br =>
      new MaeParser(br.mkString).Mae.run() match {
        case Success(result) =>
          val idx = result.indexWhere {
            case MaeObject(`objName`, _) => true
            case _ => false
          }

          val obj = result(idx).asInstanceOf[MaeObject]
          val resultMae = result.updated(idx, obj.copy(fields = obj.fields + (MaeString(key) -> MaeString(value))))
          MaePrinter.print(out, resultMae)
          out.close()
        case Failure(e) => throw e
      }
    }
}