package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Sep.  9, 2012
 * @version Aug.  8, 2013
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprParserSpec extends WordSpec with ShouldMatchers {
  "SExprParser" should {
    "parse" that {
      "empty parenthes" in {
        val s = SExprParser("()")
        s should equal (SNil)
      }
      "empty parenthes with space" in {
        val s = SExprParser("( )")
        s should be (SNil)
      }
      "nil" in {
        val s = SExprParser("nil")
        s should be (SNil)
      }
      "simple list" in {
        val s = SExprParser("(abc)")
        s should be (SCell(SAtom("abc"), SNil))
      }
    }
  }

  "Comma Separated SExpr" should {
    "parse" that {
      "simple list" in {
        val s = SExprParser("(abc,xyz)")
        s should be (SCell(SAtom("abc"), SCell(SAtom("xyz"), SNil)))
      }
    }
  }
}
