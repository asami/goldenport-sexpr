package org.goldenport.sexpr.eval

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import play.api.libs.json._
import org.goldenport.sexpr._

/*
 * @since   Aug. 17, 2018
 * @version Sep. 20, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LispEvaluatorSpec extends WordSpec with Matchers with GivenWhenThen {
  val config = LispConfig.debug
  "expression" should {
    "flat" in {
      val evaluator = LispEvaluator(config)
      val r = evaluator.eval("(and t nil)")
      r should be(SNil)
    }
    "nest" in {
      val evaluator = LispEvaluator(config)
      val r = evaluator.eval("(and t (or t 1 nil))")
      r should be(SBoolean.TRUE)
    }
    "plus" in {
      val evaluator = LispEvaluator(config)
      val r = evaluator.eval("(+ 1 2 3)")
      r should be(SNumber(6))
    }
  }
  "literal" should {
    "json" in {
      val evaluator = LispEvaluator(config)
      val r = evaluator.eval("""{"a":"b"}""")
      r should be(SJson("""{"a":"b"}"""))
    }
    // "jexl" in {
    //   val evaluator = LispEvaluator(config)
    //   val r = evaluator.eval("""jexl[1 + 2 + 3]""")
    //   r should be(SNumber(6))
    // }
    // "xpath" in {
    //   val evaluator = LispEvaluator(config)
    //   val r = evaluator.eval("""/a/b/c""")
    //   r should be(SPath("/a/b/c"))
    // }
    // "xpath literal" in {
    //   val evaluator = LispEvaluator(config)
    //   val r = evaluator.eval("""xpath"/a/b/c"""")
    //   r should be(SPath("/a/b/c"))
    // }
    // "regex" in {
    //   val evaluator = LispEvaluator(config)
    //   val r = evaluator.eval("""regex"[a-z]+"""")
    //   r should be(SRegex("""[a-z]+""".r))
    // }
  }
}
