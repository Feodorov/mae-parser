package com.github.feodorov.mae

import org.parboiled2._

import scala.collection.immutable.Seq

/**
  * @author kfeodorov 
  * @since 07.04.16
  */

class MaeParser(val input: ParserInput) extends Parser {
  import MaeParser._

  private[this] case class MaeStringsBlock(lines: Seq[MaeString])

  def Mae = rule {
    WhiteSpace ~ zeroOrMore(MaeObj).separatedBy(NlWs) ~ EOI
  }

  private[this] def MaeObj = rule {
    MaeStr ~ OpenBracket ~
      MaeStrBlock ~
      ColonSeparator ~
      MaeStrBlock ~
      zeroOrMore(MaeArr) ~
      CloseBracket ~>
      ((name: MaeString, keys: MaeStringsBlock, values: MaeStringsBlock, arrays: Seq[MaeArray]) => {
        //first array name is captured as last string of MaeStrBlock => fix it
        val firstArrName = values.lines.drop(keys.lines.size).map(_.value).headOption.getOrElse("")
        arrays.foldLeft {
          MaeObject(
            name.value.trim(),
            keys.lines.zip(values.lines).toMap[MaeValue, MaeValue]
          )
        } { case (acc, arr @ MaeArray(arrName, _, _)) =>
          val arrayName = if (arrName.isEmpty) firstArrName else arrName
          acc.copy(fields = acc.fields + (MaeString(arrayName) -> arr.copy(name = arrayName)))
        }
      }
    )
  }

  private[this] def MaeArr = rule {
    MaeStr ~ SquareOpenBracket ~ oneOrMore(CharPredicate.Digit) ~ SquareCloseBracket ~ WhiteSpace ~ OpenBracket ~ NlWs ~
      MaeStrBlock ~
      ColonSeparator ~
      MaeStrBlock ~
      ColonSeparator ~
      NlWs ~ CloseBracket ~ NlWs ~>
      ((name: MaeString, keys: MaeStringsBlock, values: MaeStringsBlock) =>
        MaeArray(name.value.trim(), keys.lines, values.lines))
  }

  private[this] def MaeStrBlock = rule {
    oneOrMore(MaeStr).separatedBy(Newline) ~>
      ((p: Seq[MaeString]) => MaeStringsBlock(p.filter(_.value.nonEmpty)))
  }

  private[this] def MaeStr = rule {
    NlWs ~ capture(!ColonSeparator ~ zeroOrMore(LineChars) ~ WhiteSpace) ~>
      (s => MaeString(s.trim()))
  }

  private[this] def ColonSeparator = rule { NlWs ~ 3.times(Colon) ~ WhiteSpace }
  private[this] def Newline = rule { quiet(optional('\r') ~ '\n') }
  private[this] def WhiteSpace = rule { quiet(zeroOrMore(WhiteSpaceChar)) }
  private[this] def NlWs = rule { quiet(zeroOrMore(NewlineOrWhiteSpace)) }
}

object MaeParser {
  val WhiteSpaceChar = CharPredicate(" \t\f")
  val NewlineOrWhiteSpace = CharPredicate(" \n\r\t\f")
  val OpenBracket = CharPredicate('{')
  val CloseBracket = CharPredicate('}')
  val SquareOpenBracket = CharPredicate('[')
  val SquareCloseBracket = CharPredicate(']')
  val Colon = CharPredicate(':')
  val LineChars = CharPredicate.Printable -- OpenBracket -- CloseBracket -- SquareOpenBracket -- SquareCloseBracket
}