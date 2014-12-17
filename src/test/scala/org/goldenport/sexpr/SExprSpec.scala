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
 * @version Dec. 17, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprSpec extends WordSpec with Matchers {
  import org.goldenport.sexpr.SExpr.Implicits._

  "SExpr" should {
    "list" which {
      "dropWhile" in {
        var s = SExprParser("(:k 100)")
        val r = s.toList.get.dropWhile(_ match {
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
