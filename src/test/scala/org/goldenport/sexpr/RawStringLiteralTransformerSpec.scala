package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Sep. 14, 2014
 * @version Sep. 14, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class RawStringLiteralTransformerSpec extends WordSpec with Matchers {
  "RawStringLiteralTransformerSpec" should {
    "typical" in {
      val s = RawStringLiteralTransformer.transform("\"\"\"abc\ndef\n\"\"\"")
      s should be ("\"abc\\ndef\\n\"")
    }
  }
}
