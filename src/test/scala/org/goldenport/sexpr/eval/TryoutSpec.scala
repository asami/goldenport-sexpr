package org.goldenport.sexpr.eval

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import play.api.libs.json._
import org.goldenport.i18n.I18NContext
import org.goldenport.sexpr._

/*
 * @since   Oct.  1, 2019
 * @version Oct.  1, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  val config = LispConfig.trace
  val i18n = I18NContext.default // XXX test

  // "expression" should {
  //   "nest" in {
  //     val evaluator = LispEvaluator(config, i18n)
  //     // val r = evaluator.eval("(and t (or t 1 nil))")
  //     val r = evaluator.eval("(and t)")
  //     // val r = evaluator.eval("t")
  //     r should be(SBoolean.TRUE)
  //   }
  // }
}
