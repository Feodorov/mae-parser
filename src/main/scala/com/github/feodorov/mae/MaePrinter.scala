package com.github.feodorov.mae

import java.io.{BufferedOutputStream, OutputStream, PrintWriter}


/**
  * @author kfeodorov 
  * @since 13.04.16
  */
object MaePrinter {

  def print(out: OutputStream, mae: Seq[MaeValue]) {
    implicit val pw = new PrintWriter(new BufferedOutputStream(out))
    mae.foreach(print(_))
    pw.flush()
  }

  private def print(mae: MaeValue, offset: String = "")(implicit pw: PrintWriter) {
    mae match {
      case s: MaeString => pw.println(s"$offset${s.value}")

      case o: MaeObject =>
        val (arrs, kvs) = o.fields.partition(_._2.isInstanceOf[MaeArray])
        val keys = kvs.keys

        pw.println(s"${o.name} {".trim())

        keys.foreach(k => print(k, " "))
        if (kvs.nonEmpty) pw.println(" :::")
        keys.map(kvs).foreach(k => print(k, " "))

        arrs.foreach(p => print(p._2, " "))

        pw.println("}")
        pw.println()

      case a: MaeArray =>
        val elementsSize = a.elements.count {
          case MaeString(s) if s.trim.startsWith("#") => false //comment line
          case _ => true
        }

        pw.println(s"$offset${a.name}[$elementsSize] {")

        a.header.foreach(s => print(s, s"$offset "))
        if (a.header.nonEmpty) pw.println(s"$offset :::")
        a.elements.foreach(s => print(s, s"$offset "))
        if (a.header.nonEmpty) pw.println(s"$offset :::")

        pw.println(s"$offset}")
    }
  }

}
