package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Jan. 27, 2019
 *  version Feb.  3, 2019
 *  version Apr. 13, 2019
 * @version Oct.  1, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers {
  import org.goldenport.sexpr.SExpr.Implicits._
  // "SExprParser" should {
  //   "json" which {
  //     "typical" in {
  //       val s = SExprParserNew("json\"\"\"{\"a\":1}\"\"\"")
  //       s should equal (LogicalTokens(Vector(RawStringToken("""{"a":1}"""))))
  //     }
  //   }
  // }
}
