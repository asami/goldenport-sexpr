package org.goldenport.sexpr.eval

import org.goldenport.sexpr._
import org.goldenport.sexpr.SExprConverter._

/*
 * @since   Sep. 25, 2018
 * @version Sep. 25, 2018
 * @author  ASAMI, Tomoharu
 */
case class Parameters(parameters: List[SExpr]) {
  def parameter1[A](
    spec: FunctionSpecification
  )(implicit A: SExprConverter[A]) = A.apply(parameters(0))
  def parameter2[A, B](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A], b: SExprConverter[B]): (A, B) = ???
  def parameter3[A, B, C](spec: FunctionSpecification): (A, B, C) = ???

  def asBigDecimalList: List[BigDecimal] = parameters.map(_.asBigDecimal)
}
