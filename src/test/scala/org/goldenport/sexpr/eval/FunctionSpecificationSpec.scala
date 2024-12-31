package org.goldenport.sexpr.eval

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Jun. 19, 2021
 * @version Sep.  7, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class FunctionSpecificationSpec extends WordSpec with Matchers with GivenWhenThen {
  protected final def param_argument(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.argument(name)

  protected final def param_argument_option(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.argumentOption(name)

  "a" should {
    "b" which {
      "c" in {
        val s = FunctionSpecification(
          "test",
          FunctionSpecification.Parameters(
            param_argument("x"),
            param_argument_option("y")
          )
        )
        val params = Parameters(List(SString("a"), SString("b")))
        val r = s.resolve(params)
        // println(r)
        r.getProperty("x") should be(Some(SString("a")))
        r.getProperty("y") should be(Some(SString("b")))
      }
      "cc" in {
        val s = FunctionSpecification(
          "test",
          FunctionSpecification.Parameters(
            param_argument("x"),
            param_argument_option("y")
          )
        )
        val params = Parameters(List(SString("a")))
        val r = s.resolve(params)
        // println(r)
        r.getProperty("x") should be(Some(SString("a")))
        r.getProperty("y") should be(None)
      }
    }
  }
}
