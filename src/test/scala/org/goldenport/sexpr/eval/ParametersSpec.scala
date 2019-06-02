package org.goldenport.sexpr.eval

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import play.api.libs.json._
import org.goldenport.sexpr._

/*
 * @since   May.  3, 2019
 * @version May.  4, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ParametersSpec extends WordSpec with Matchers with GivenWhenThen {
  val config = LispConfig.debug
  "keyword" should {
    "arguments" in {
      val a = List(SKeyword("type"), SString("string"), SString("one"), SString("two"))
      val ps = Parameters(a)
      ps should be(Parameters(
        List(SString("one"), SString("two")),
        Map('type -> SString("string")),
        Set.empty
      ))
    }
  }
}
