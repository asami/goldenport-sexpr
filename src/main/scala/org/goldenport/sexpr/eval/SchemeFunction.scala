package org.goldenport.sexpr.eval

import org.goldenport.sexpr._

/*
 * @since   Mar. 17, 2019
 * @version Mar. 22, 2019
 * @author  ASAMI, Tomoharu
 */
object SchemeFunction {
  val functions = Vector(Define)

  case object Define extends ControlFunction {
    val specification = FunctionSpecification("define", 2)
    def apply(p: LispContext) = {
      val (atom, v) = p.parameters.argument2[SAtom, SExpr](specification)
      ???
      // val r = p.eval(v)
      // p.toResult(r, Record.data(atom.name -> r))
    }
  }
}
