package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.net.URL
import org.goldenport.exception.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.http.{Request, Response}
import org.goldenport.io.MimeType
import org.goldenport.sexpr._, SExprConverter._

/*
 * @since   Sep. 10, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
trait LispFunction extends PartialFunction[LispContext, LispContext] {
  def specification: FunctionSpecification
  def name: String = specification.name

  def isDefinedAt(p: LispContext): Boolean = {
    val r = is_defined_at(p)
    p.log.trace(s"isDefinedAt($name: ${p.value}): $r")
    r
  }

  protected def is_defined_at(p: LispContext): Boolean = p.value match {
    case m: SCell => p.args match {
      case Nil => false
      case x :: xs => x match {
        case a: SAtom if (a.name == name) => true
        case a: SAtom =>
          p.log.trace(s"$name, ${a.name}")
          false
        case _ => false
      }
    }
    case _ => false
  }

  def apply(p: LispContext): LispContext
}

trait EvalFunction extends LispFunction {
  def apply(p: LispContext): LispContext = {
    val r = eval(p.parameters)
    p.toResult(r)
  }

  def eval(p: Parameters): SExpr
}

trait ControlFunction extends LispFunction {
}

trait HeavyFunction extends LispFunction { // CPU bound
}

trait EffectFunction extends LispFunction {
  def applyEffect(p: LispContext): UnitOfWorkFM[LispContext]
}

trait IoFunction extends EffectFunction { // I/O bound
}

object EvalFunction {
  case object Car extends EvalFunction {
    val specification = FunctionSpecification("car", 1)
    def eval(p: Parameters) = p.parameters.head
  }

  case object And extends EvalFunction {
    val specification = FunctionSpecification("and")
    def eval(p: Parameters) = {
      @annotation.tailrec
      def go(x: List[SExpr]): SExpr = x match {
        case Nil => SBoolean.TRUE
        case x :: Nil => if (x.isNilOrFalse) SNil else x
        case x :: xs => if (x.isNilOrFalse) SNil else go(xs)
      }
      go(p.parameters)
    }
  }

  case object Or extends EvalFunction {
    val specification = FunctionSpecification("or")
    def eval(p: Parameters) = {
      @annotation.tailrec
      def go(x: List[SExpr]): SExpr = x match {
        case Nil => SNil
        case x :: Nil => if (x.isNilOrFalse) SNil else x
        case x :: xs => if (x.isNilOrFalse) go(xs) else x
      }
      go(p.parameters)
    }
  }

  case object Plus extends EvalFunction {
    val specification = FunctionSpecification("+")
    def eval(p: Parameters) = {
      SNumber(p.asBigDecimalList.sum)
    }
  }

  case object Length extends EvalFunction {
    val specification = FunctionSpecification("length", 1)
    def eval(p: Parameters) = {
      val r = p.parameters.map {
        case m: SString => m.string.length
        case m => m.asString.length
      }.sum
      SNumber(r)
    }
  }

  case object Pop extends LispFunction {
    val specification = FunctionSpecification("pop")
    def apply(p: LispContext): LispContext = p.pop
  }

  case object Transform extends EvalFunction {
    val specification = FunctionSpecification("transform")

    override def isDefinedAt(p: LispContext): Boolean =
      p.value.isInstanceOf[SXslt] || is_defined_at(p)

    def eval(p: Parameters) = {
      ???
    }
  }

  case object Fetch extends IoFunction {
    val specification = FunctionSpecification("fetch", 1)

    override def isDefinedAt(p: LispContext): Boolean =
      p.value.isInstanceOf[SUrl] || is_defined_at(p)

    def apply(p: LispContext): LispContext = {
      val url = p.parameters.parameter1[URL](specification) // TODO file
      val req = Request(url)
      try {
        val res = p.serviceLogic.httpService(req)
        if (res.isSuccess)
          p.toResult(res)
        else
          p.toResult(SError(req, res))
      } catch {
        case NonFatal(e) => p.toResult(SError(req, e))
      }
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpGet extends IoFunction {
    val specification = FunctionSpecification("http-get")

    def apply(p: LispContext): LispContext = RAISE.notImplementedYetDefect

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }
}
