package org.goldenport.sexpr.script

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Sep.  7, 2024
 *  version Sep. 10, 2024
 * @version Oct.  1, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  // "Lietral" should {
  //   "xpath" in {
  //     val s = """path-get /user/city <user><name>taro</name><city>yokohama</city></user>"""
  //     val r = Script.parse(s)
  //     r should be(Script(SList(SAtom("path-get"), SXPath("/user/city"), SXml("<user><name>taro</name><city>yokohama</city></user>"))))
  //   }
  // }
}

