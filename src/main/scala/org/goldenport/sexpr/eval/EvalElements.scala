package org.goldenport.sexpr.eval

import org.goldenport.sexpr._

/*
 * @since   Sep. 21, 2018
 * @version Sep. 25, 2018
 * @author  ASAMI, Tomoharu
 */
case class EvalElements(elements: List[SExpr]) {
  lazy val parameters: Parameters = Parameters(elements.tail)

  def isFunction(name: String): Boolean = elements.headOption.map {
    case SAtom(a) => a == name
  }.getOrElse(false)
}

object EvalElements {
  def apply(p: SExpr): EvalElements = p match {
    case m: SCell => EvalElements(m.list)
    case _ => ???
  }
}
