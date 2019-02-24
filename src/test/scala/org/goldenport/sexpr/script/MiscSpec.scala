package org.goldenport.sexpr.script

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Oct. 27, 2018
 *  version Oct. 27, 2018
 * @version Feb.  3, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class MiscSpec extends WordSpec with Matchers with GivenWhenThen {
  "Misc" should {
    "misc" in {
      val s = """http-post "commerce_user_cart?with=id" :form '(
  "product_id=system-system-1513216956590-apparelcloud.product-26a6fa29-2ee5-481d-b5eb-28a30960ed81"
  ("unitCount" . 1)
  "unitPrice=100")"""
      val r = Script.parse(s)
      r should be(Script(Vector(
        SList(
          SAtom("http-post"),
          SString("commerce_user_cart?with=id"),
          SKeyword("form"),
          SList(
            SAtom("quote"),
            SList(SString("product_id=system-system-1513216956590-apparelcloud.product-26a6fa29-2ee5-481d-b5eb-28a30960ed81"), SCell(SString("unitCount"), SNumber(1)), SString("unitPrice=100"))
          )
        )
      )))
    }
  }
}
