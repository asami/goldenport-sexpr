package org.goldenport.sexpr.eval

import org.goldenport.RAISE
import org.goldenport.sexpr._
import LispFunction.PathGet._

/*
 * @since   May. 25, 2019
 *  version May. 26, 2019
 * @version Mar.  6, 2021
 * @author  ASAMI, Tomoharu
 */
trait XPathPart { self: LispFunction =>
  protected final def xpath_traverse_auto(xpath: String, target: SExpr): SExpr = {
    val rt = ReturnType.autoType
    traverse(rt, SXPath(xpath), target)
  }

  protected final def xpath_traverse_node(xpath: String, target: SExpr): SXml = {
    val rt = ReturnType.nodeType
    traverse(rt, SXPath(xpath), target) match {
      case m: SXml => m
      case SNil => SError.notFound("XPath", xpath).RAISE
      case m => RAISE.noReachDefect(m.toString)
    }
  }
}
