package org.goldenport.sexpr.util

import com.asamioffice.goldenport.text.UJavaString

/*
 * @since   Mar.  7, 2021
 * @version Mar.  7, 2021
 * @author  ASAMI, Tomoharu
 */
object SExprTextMaker {
  def stringLiteral(p: String): String = '"' + escape(p) + '"'

  def escape(p: String): String = UJavaString.escapeJavaText(p)
}
