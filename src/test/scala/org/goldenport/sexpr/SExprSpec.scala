package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Sep.  9, 2012
 *  version Sep.  9, 2012
 *  version Aug.  8, 2013
 *  version Feb.  4, 2014
 *  version Dec. 17, 2014
 *  version Mar. 11, 2015
 * @version Sep. 16, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprSpec extends WordSpec with Matchers {
  import org.goldenport.sexpr.SExpr.Implicits._

  "SExpr" should {
    "literal" which {
      "backslash" in {
        val rawtext = """abc \9 xyz"""
        val text = """abc \\9 xyz"""
        val literal = "\"" + text + "\""
        var s = SExprParser(literal)
        s should equal (SString(rawtext))
        s.show should equal (literal)
      }
      "backslash in expr" in {
        val rawtext = """abc \9 xyz"""
        val text = """abc \\9 xyz"""
        val literal = "(\"" + text + "\")"
        var s = SExprParser(literal)
        s should equal (SList(SString(rawtext)))
        s.show should equal (literal)
      }
      "invalid backslash" in {
        val text = """abc \9 xyz"""
        val literal = "(\"" + text + "\")"
        intercept[IllegalArgumentException] {
          SExprParser(literal)
        }
      }
    }
    "list" which {
      "dropWhile" in {
        var s = SExprParser("(:k 100)")
        val r = s.getList.get.dropWhile(_ match {
          case _: SKeyword => false
          case _ => true
        })
        r should equal (List(SKeyword("k"), SNumber("100")))
      }
    }
    "keyword" which {
      "string" in {
        var s = SExprParser("""(:k "value")""")
        var v = SExpr.getKeyword[String](s, "k")
        v should equal (Some("value"))
      }
      "string list" in {
        var s = SExprParser("""(:k ("value"))""")
        var v = SExpr.getKeyword[List[String]](s, "k")
        v should equal (Some(List("value")))
      }
      "strings list" in {
        var s = SExprParser("""(:k ("value1" "value2"))""")
        var v = SExpr.getKeyword[List[String]](s, "k")
        v should equal (Some(List("value1", "value2")))
      }
    }
  }
}
