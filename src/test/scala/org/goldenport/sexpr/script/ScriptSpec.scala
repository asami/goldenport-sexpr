package org.goldenport.sexpr.script

import org.scalatest.{WordSpec, GivenWhenThen}
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.sexpr._

/*
 * @since   Sep.  3, 2018
 * @version Sep. 25, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ScriptSpec extends WordSpec with Matchers with GivenWhenThen {
  "Script" should {
    "literal" in {
      val s = """1"""
      val r = Script.parse(s)
      r should be(Script(SExprParser(s)))
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
  }
}
