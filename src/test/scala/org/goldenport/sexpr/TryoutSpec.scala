package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Jan. 27, 2019
 * @version Feb.  3, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers {
  import org.goldenport.sexpr.SExpr.Implicits._

  // "SExprSpec" should {
  //   "quote" which {
  //     "typical" in {
  //       val s = SExprParserNew("'a")
  //       s should equal (SList(SAtom("quote"), SAtom("a")))
  //     }
  //   }
  // }
}
