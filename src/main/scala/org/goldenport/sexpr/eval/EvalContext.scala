package org.goldenport.sexpr.eval

import org.goldenport.exception.RAISE
import org.goldenport.config.ConfigHelper
import org.goldenport.record.v3.Record
import org.goldenport.record.http.Response
import org.goldenport.sexpr._

/*
 * @since   Feb. 27, 2014
 *  version Sep. 18, 2014
 *  version Aug. 20, 2018
 * @version Sep. 25, 2018
 * @author  ASAMI, Tomoharu
 */
trait EvalContext extends ConfigHelper {
  def config: EvalConfig
  def value: SExpr
  def bindings: Record

  def toResult(p: EvalContext): EvalContext = toResult(p.value, p.bindings)
  def toResult(expr: SExpr): EvalContext = toResult(expr, Record.empty)
  def toResult(expr: SExpr, bindings: Record): EvalContext

  lazy val valueResolved = value match {
    case m: SControl => m.resolve
    case _ => value
  }
  def resolve: EvalContext = value match {
    case m: SControl => toResult(m.resolve)
    case _ => this
  }

  // obsolate (misleading semantics)
  lazy val args: List[SExpr] = {
    value match {
      case xs: SList => xs.list
      case _ => RAISE.noReachDefect(value.toString)
    }
  }

  // obsolate (misleading semantics, use evalElements instead.)
  lazy val arg: SExpr = {
    value match {
      case xs: SList => xs.list.head
      case _ => value
    }
  }

  lazy val evalElements: EvalElements = EvalElements(value)
  lazy val parameters: Parameters = evalElements.parameters

  // lazy val functionParameters: List[SExpr] = value match {
  //   case SNil => Nil
  //   case m: SList => m.list.tail.map(_.resolve)
  //   case m => List(m.resolve)
  // }

  def toSExpr(p: Response): SExpr = config.toSExpr(p)

  import SExprConverters._

  def argString: String = {
    SExprConverters.toString(arg)
  }

  def argBoolean: Boolean = {
    ???
  }

  def argInt: Int = {
    ???
  }

  def argLong: Long = {
    ???
  }

  def argFloat: Float = {
    ???
  }

  def argDouble: Double = {
    ???
  }

  def argStrings: Seq[String] = {
    toStrings(args)
  }

  def argBooleans: Seq[Boolean] = {
    ???
  }

  def argInts: Seq[Int] = {
    ???
  }

  def argLongs: Seq[Long] = {
    ???
  }

  def argFloats: Seq[Float] = {
    ???
  }

  def argDoubles: Seq[Double] = {
    ???
  }

  def toResult(v: Boolean): EvalContext = {
    toResult(fromBoolean(v))
  }

  def toResult(v: Int): EvalContext = {
    toResult(fromInt(v))
  }

  def toResult(v: Long): EvalContext = {
    toResult(fromLong(v))
  }

  def toResult(v: Float): EvalContext = {
    toResult(fromFloat(v))
  }

  def toResult(v: Double): EvalContext = {
    toResult(fromDouble(v))
  }

  def toResult(b: Option[Boolean], d: Boolean): EvalContext = {
    toResult(fromBoolean(b.getOrElse(d)))
  }

  def toResult(b: Option[Int], d: Int): EvalContext = {
    toResult(fromInt(b.getOrElse(d)))
  }

  def toResult(b: Option[Long], d: Long): EvalContext = {
    toResult(fromLong(b.getOrElse(d)))
  }

  def toResult(b: Option[Float], d: Float): EvalContext = {
    toResult(fromFloat(b.getOrElse(d)))
  }

  def toResult(b: Option[Double], d: Double): EvalContext = {
    toResult(fromDouble(b.getOrElse(d)))
  }

  def toResult(p: Response): EvalContext = toResult(toSExpr(p))
}
