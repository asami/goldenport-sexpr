package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Sep.  9, 2012
 *  version Sep.  9, 2012
 * @version Aug.  8, 2013
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprSpec extends WordSpec with ShouldMatchers {
  import org.goldenport.sexpr.SExpr._

  "SExpr" should {
    "list" that {
      "dropWhile" in {
        var s = SExprParser("(:k 100)")
        val r = s.toList.get.dropWhile(_ match {
          case _: SKeyword => false
          case _ => true
        })
        r should equal (List(SKeyword("k"), SNumber("100")))
      }
    }
    "keyword" that {
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
