package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Sep.  9, 2012
 *  version Aug.  9, 2013
 * @version Feb.  4, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprParserSpec extends WordSpec with Matchers {
  "SExprParser" should {
    "parse" which {
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
      "日本語" in {
        val s = SExprParser("(日本語)")
        s should be (SCell(SAtom("日本語"), SNil))
      }
    }
  }

  "Comma Separated SExpr" should {
    "parse" which {
      "simple list" in {
        val s = SExprParser("(abc,xyz)")
        s should be (SCell(SAtom("abc"), SCell(SAtom("xyz"), SNil)))
      }
    }
  }
}
