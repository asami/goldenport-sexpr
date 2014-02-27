package org.goldenport.sexpr.eval

import org.goldenport.sexpr._

/*
 * @since   Feb. 27, 2014
 * @version Feb. 27, 2014
 * @author  ASAMI, Tomoharu
 */
trait EvalContext {
  def value: SExpr
  def toResult(expr: SExpr): EvalContext

  def args: List[SExpr] = {
    value match {
      case xs: SList => xs.list
      case _ => sys.error("???")
    }
  }
}
