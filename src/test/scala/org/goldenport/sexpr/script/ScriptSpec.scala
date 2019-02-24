package org.goldenport.sexpr.script

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Sep.  3, 2018
 *  version Sep. 25, 2018
 *  version Oct. 26, 2018
 *  version Jan.  1, 2019
 * @version Feb.  9, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ScriptSpec extends WordSpec with Matchers with GivenWhenThen {
  "Literal" should {
    "number" in {
      val s = """1"""
      val r = Script.parse(s)
      r should be(Script(SExprParser(s)))
    }
    "keyword" in {
      val s = """:key"""
      val r = Script.parse(s)
      r should be(Script(SKeyword("key")))
    }
    "expression" in {
      val s = """(+ 1 1)"""
      val r = Script.parse(s)
      r should be(Script(SExprParser(s)))
    }
    "default script" in {
      val s = """[1 + 2 + 3]"""
      val r = Script.parse(s)
      r should be(Script(SScript(None, "1 + 2 + 3")))
    }
    "javascript" in {
      val s = """javascript[1 + 2 + 3]"""
      val r = Script.parse(s)
      r should be(Script(SScript(Some("javascript"), "1 + 2 + 3")))
    }
    "jexl" in {
      val s = """jexl[1 + 2 + 3]"""
      val r = Script.parse(s)
      r should be(Script(SScript(Some("jexl"), "1 + 2 + 3")))
    }
    "xpath literal" in {
      val s = """/a/b/c"""
      val r = Script.parse(s)
      r should be(Script(SXPath("/a/b/c")))
    }
    "xpath literal 2" in {
      val s = """/a/b/c[id='city']"""
      val r = Script.parse(s)
      r should be(Script(SXPath("/a/b/c[id='city']")))
    }
    "xpath" in {
      val s = """xpath"/a/b/c""""
      val r = Script.parse(s)
      r should be(Script(SXPath("/a/b/c")))
    }
    "regex" in {
      val s = """regex"[a-z]+""""
      val r = Script.parse(s)
      r should be(Script(SRegex("""[a-z]+""".r)))
    }
    "json" in {
      val s = """{"user": {"name":"taro", "city":"yokohama"}}"""
      val r = Script.parse(s)
      r should be(Script(SJson("""{"user": {"name":"taro", "city":"yokohama"}}""")))
    }
  }
  "expression" should {
    "quote" in {
      val s = """'a"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom.quote, SAtom("a"))))
    }
    "association" in {
      val s = """("a" . 1)"""
      val r = Script.parse(s)
      r should be(Script(SCell(SString("a"), SNumber(1))))
    }
    "with empty list" in {
      val s = """a ()"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom("a"), SNil)))
    }
    "s-expression nesting" in {
      val s = """(())"""
      val r = Script.parse(s)
      r should be(Script(SList(SNil)))
    }
    "s-expression deep nesting" in {
      val s = """(+ (* 5 3) 2)"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom("+"), SList(SAtom("*"), SNumber(5), SNumber(3)), SNumber(2))))
    }
    "s-expression nesting with space" in {
      val s = """("a"
  "b")
"""
      val r = Script.parse(s)
      r should be(Script(SList(SString("a"), SString("b"))))
    }
    "s-expression with keyword" in {
      val s = """a :form (
  "a"
  "b")
"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom("a"), SKeyword("form"), SList(SString("a"), SString("b")))))
    }
    "s-expression with quote" in {
      val s = """a "b" :form '(
  "a"
  "b")
"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom("a"), SString("b"), SKeyword("form"),
        SList(SAtom.quote, SList(SString("a"), SString("b")))
      )))
    }
    "s-expression with quote and association" in {
      val s = """a "b" :form '(
  "a"
  ("z" . 1)
  "b")
"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom("a"), SString("b"), SKeyword("form"),
        SList(SAtom.quote, SList(SString("a"), SCell(SString("z"), SNumber(1)), SString("b")))
      )))
    }
    "xpath" ignore {
      val s = """pathget /user/city <user><name>taro</name><city>yokohama</city></user>"""
      val r = Script.parse(s)
      r should be(Script(SList(SAtom("path-get"), SXPath("/user/city"), SXml("<user><name>taro</name><city>yokohama</city></user>"))))
    }
  }
}
