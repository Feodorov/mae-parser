package com.github.feodorov.mae

/**
  * @author kfeodorov 
  * @since 07.04.16
  */
import org.parboiled2._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class MaeParserTest extends FlatSpec with Matchers {

  "The MaeParser" should "parse" in {
    parse(scala.io.Source.fromInputStream(getClass().getResourceAsStream("/example.mae")).mkString) shouldBe
      Seq(
        MaeObject(
          name = "",
          fields = Map(MaeString("s_m_m2io_version") -> MaeString("2.0.0"))
        ),
        MaeObject(
          "f_m_ct",
          Map(
            MaeString("r_yet_another_value") -> MaeString("42"),
            MaeString("s_m_title") -> MaeString("PROJECT"),
            MaeString("s_m_entry_id") -> MaeString("12345"),
            MaeString("m_depend") -> MaeArray(
              name = "m_depend",
              header = Seq(
                MaeString("# First column is dependency index #"),
                MaeString("i_m_depend_dependency"),
                MaeString("s_m_depend_property")
              ),
              elements = Seq(
                MaeString("1 10 i_prop_1"),
                MaeString("2 10 i_prop_2")
              )
            ),
            MaeString("m_atom") -> MaeArray(
              name = "m_atom",
              header = Seq(
                MaeString("# First column is atom index #"),
                MaeString("r_m_x_coord"),
                MaeString("r_m_y_coord"),
                MaeString("r_m_z_coord"),
                MaeString("s_m_chain_name")
              ),
              elements = Seq(
                MaeString("1 -1.234567 -2.34567 -4.567890 X"),
                MaeString("2 -1.234567 -2.34567 -4.567890 Y"),
                MaeString("3 -1.234567 -2.34567 -4.567890 Z")
              )
            )
          )
        )
      )
  }

  def parse(s: String) = {
    val parser = new MaeParser(s)
    parser.Mae.run() match {
      case Success(result) => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => throw e
    }
  }
}