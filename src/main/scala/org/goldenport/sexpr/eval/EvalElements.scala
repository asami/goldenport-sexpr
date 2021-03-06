package org.goldenport.sexpr.eval

import org.goldenport.RAISE
import org.goldenport.sexpr._

/*
 * @since   Sep. 21, 2018
 *  version Oct.  8, 2018
 *  version Feb. 25, 2019
 * @version Mar.  4, 2019
 * @author  ASAMI, Tomoharu
 */
case class EvalElements(elements: List[SExpr]) {
  lazy val functionName = elements match {
    case SAtom(name) :: _ => name
    case _ => RAISE.syntaxErrorFault(elements.toString)
  }
  lazy val parameters: Parameters = Parameters(elements.tail)

  def isFunction(name: String): Boolean = elements.headOption.collect {
    case SAtom(a) => a == name
  }.getOrElse(false)

  def show = elements.map(_.show).mkString("(", " ", ")")
}

object EvalElements {
  def apply(p: SExpr): EvalElements = p match {
    case m: SCell => EvalElements(m.list)
    case m => RAISE.unsupportedOperationFault(s"${p.show}")
  }
}
