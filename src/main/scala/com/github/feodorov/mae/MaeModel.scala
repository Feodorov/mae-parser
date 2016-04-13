package com.github.feodorov.mae

/**
  * @author kfeodorov 
  * @since 07.04.16
  */

sealed abstract class MaeValue

case class MaeObject(name: String, fields: Map[MaeValue, MaeValue]) extends MaeValue

case class MaeArray(name: String, header: Seq[MaeValue], elements: Seq[MaeValue]) extends MaeValue

case class MaeString(value: String) extends MaeValue

object MaeString {
  implicit def fromString(s: String): MaeString = MaeString(s)
}