package org.goldenport.sexpr.eval.sci

import org.goldenport.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Sep. 15, 2019
 * @version Sep. 15, 2019
 * @author  ASAMI, Tomoharu
 */
object SciFunction {
  val functions = Vector(
    PearsonR
  )

  case object PearsonR extends IoFunction {
    val specification = FunctionSpecification("pearsonr", 1)

    def apply(p: LispContext): LispContext = {
      val r: SExpr = ???
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }
}
