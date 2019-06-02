package org.goldenport.sexpr.eval

import org.goldenport.RAISE
import org.goldenport.record.v3.Record
import org.goldenport.sexpr._

/*
 * @since   Mar. 17, 2019
 * @version Mar. 23, 2019
 * @author  ASAMI, Tomoharu
 */
object EmacsLispFunction {
  val functions = Vector(Defun)

  case object Defun extends ControlFunction {
    val specification = FunctionSpecification("defun", 3)
    def apply(p: LispContext) = {
      val name = p.parameters.argumentZeroBased(0) match {
        case m: SAtom => m.name
        case m => RAISE.invalidArgumentFault(s"Invalid function atom: ${m.show}")
      }
      val params = p.parameters.argumentZeroBased(1) match {
        case m: SList => m.list map {
          case m: SAtom => m.name
          case m => RAISE.invalidArgumentFault(s"Invalid parameter name: ${m.show}")
        }
        case m => RAISE.invalidArgumentFault(s"Invalid parameter list: ${m.show}")
      }
      val body = p.parameters.arguments.tail.tail
      val r = SLambda(params, body)
      p.toResult(r, Record.data(name -> r))
    }
  }
}
