package org.goldenport.sexpr.eval

/*
 * @since   Sep. 21, 2018
 * @version Sep. 25, 2018
 * @author  ASAMI, Tomoharu
 */
case class FunctionSpecification(
  name: String,
  numberOfMeaningfulParameters: Int = 2
) {
}

object FunctionSpecification {
  case class Signature()
  case class Parameters()
}
