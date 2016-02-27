package org.goldenport.sexpr.eval

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Feb. 27, 2016
 * @version Feb. 27, 2016
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprParsersSpec extends WordSpec with Matchers with GivenWhenThen {
  object TestParser extends SExprParsers {
    def parse(s: String): (String, Seq[String]) = parse(SExprParser(s))
    def parse(expr: SExpr): (String, Seq[String]) = {
      val reader = SExprReader.create(expr)
      rule(reader) match {
        case Success(result, _) => result
        case failure: NoSuccess => sys.error(failure.msg)
      }
    }

    def rule: Parser[(String, Seq[String])] = property
    def property = command
    def command: Parser[(String, Seq[String])] = atom_name_string_list("command")
  }

  object Test2Parser extends SExprParsers {
    def parse(s: String): Seq[String] = parse(SExprParser(s))
    def parse(expr: SExpr): Seq[String] = {
      val reader = SExprReader.create(expr)
      rule(reader) match {
        case Success(result, _) => result
        case failure: NoSuccess => sys.error(failure.msg)
      }
    }

    def rule: Parser[Seq[String]] = {
      open ~> property.* <~ close ^^ {
        case properties =>
          get_string_vector("command", properties)
      }
    }
    def property = command
    def command: Parser[(String, Seq[String])] = atom_name_string_list("command")
  }

  "SExprParsersSpec" should {
    "typical" which {
      "typical" in {
        TestParser.parse("""(command "abc")""") should be("command" -> Vector("abc"))
      }
      "two" in {
        TestParser.parse("""(command "abc", "xyz")""") should be("command" -> Vector("abc", "xyz"))
      }
    }
    "typical2" which {
      "typical" in {
        Test2Parser.parse("""((command "abc"))""") should be(Vector("abc"))
      }
    }
  }
}
