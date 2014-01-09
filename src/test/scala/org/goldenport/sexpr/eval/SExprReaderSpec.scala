package org.goldenport.sexpr.eval

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Jan.  9, 2014
 * @version Jan.  9, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprReaderSpec extends WordSpec with ShouldMatchers with GivenWhenThen {
  "SExprReader" should {
    "nil" in {
      val expr = SExprParser.apply("nil")
      val reader = SExprReader.create(expr)
      reader.exprs.toList should be (List(SNil))
    }
    "empty list" in {
      val expr = SExprParser.apply("()")
      val reader = SExprReader.create(expr)
      reader.exprs.toList should be (List(SNil))
    }
    "one atom list" in {
      val expr = SExprParser.apply("(abc)")
      val reader = SExprReader.create(expr)
      reader.exprs.toList should be (List(SOpen, SAtom("abc"), SClose))
    }
    "two atoms list" in {
      val expr = SExprParser.apply("(abc xyz)")
      val reader = SExprReader.create(expr)
      reader.exprs.toList should be (List(SOpen, SAtom("abc"), SAtom("xyz"), SClose))
    }
    "nested list" in {
      val expr = SExprParser.apply("(abc (123 456) xyz)")
      val reader = SExprReader.create(expr)
      reader.exprs.toList should be (List(SOpen, SAtom("abc"), SOpen, SNumber("123"), SNumber("456"), SClose, SAtom("xyz"), SClose))
    }
  }
}
