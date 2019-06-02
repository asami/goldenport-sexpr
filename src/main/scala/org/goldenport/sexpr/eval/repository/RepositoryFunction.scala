package org.goldenport.sexpr.eval.repository

import org.goldenport.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Mar. 23, 2019
 * @version Mar. 30, 2019
 * @author  ASAMI, Tomoharu
 */
object RepositoryFunction {
  val functions = Vector()

  case object RepositoryGet extends IoFunction {
    val specification = FunctionSpecification("repo-get", 2)

    def apply(p: LispContext): LispContext = {
      val db = _get_database(p.parameters)
      // val r = p.parameters.argument(1) match {
      // }
      RAISE.notImplementedYetDefect
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object RepositoryQuery extends IoFunction {
    val specification = FunctionSpecification("repo-query", 1)

    def apply(p: LispContext): LispContext = {
      RAISE.notImplementedYetDefect
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object RepositoryPost extends IoFunction {
    val specification = FunctionSpecification("repo-post", 1)

    def apply(p: LispContext): LispContext = {
      RAISE.notImplementedYetDefect
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object RepositoryPut extends IoFunction {
    val specification = FunctionSpecification("repo-put", 1)

    def apply(p: LispContext): LispContext = {
      RAISE.notImplementedYetDefect
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object RepositoryDelete extends IoFunction {
    val specification = FunctionSpecification("repo-delete", 1)

    def apply(p: LispContext): LispContext = {
      RAISE.notImplementedYetDefect
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  private def _get_database(p: Parameters): Option[Symbol] = _get_property_symbol(p, 'database)

  private def _symbol_string(p: Parameters): (Symbol, String) = {
    p.argument(0)
    p.argument(1)
    RAISE.notImplementedYetDefect
  }

  private def _get_property_symbol(p: Parameters, key: Symbol): Option[Symbol] = 
    p.getProperty(key).map {
      case SString(s) => Symbol(s)
      case SAtom(s) => Symbol(s)
      case m => RAISE.invalidArgumentFault(SError.invalidDatatype(key.name, m).print)
    }
}
