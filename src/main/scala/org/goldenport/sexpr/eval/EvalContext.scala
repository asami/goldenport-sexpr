package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE
import org.goldenport.config.ConfigHelper
import org.goldenport.record.v3.{IRecord, Record, Field}
import org.goldenport.record.http.Response
import org.goldenport.sexpr._

/*
 * @since   Feb. 27, 2014
 *  version Sep. 18, 2014
 *  version Aug. 20, 2018
 *  version Sep. 25, 2018
 *  version Oct. 17, 2018
 *  version Feb. 28, 2019
 *  version Mar. 10, 2019
 *  version Feb. 29, 2020
 *  version Apr. 24, 2022
 * @version Jul. 17, 2023
 * @author  ASAMI, Tomoharu
 */
trait EvalContext extends ConfigHelper {
  def config: EvalConfig
  def value: SExpr
  def bindings: IRecord
  def incident: IncidentSequence

  def toResult(p: EvalContext): EvalContext = toResult(p.value, p.bindings, p.incident)
  def toResult(expr: SExpr): EvalContext = toResult(expr, Record.empty)
  def toResult(expr: SExpr, i: IncidentSequence): EvalContext = toResult(expr, Record.empty, i)
  def toResult(expr: SExpr, bindings: IRecord): EvalContext = toResult(expr, bindings, IncidentSequence.empty)
  def toResult(expr: SExpr, bindings: IRecord, i: IncidentSequence): EvalContext

  def addBindings(bindings: IRecord): EvalContext

  lazy val valueResolved = value match {
    case m: SControl => m.resolve
    case _ => value
  }
  def resolve: EvalContext = {
    // println(s"EvalContext#resolve $value")
    value match {
      case m: SControl => toResult(m.resolveContext)
      case _ => this
    }
  }

  def normalizeSExpr(p: SExpr): Option[SExpr] = None

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

  lazy val valueOrParameters: \/[SExpr, Parameters] = value match {
    case m: SCell => \/-(parameters)
    case m => -\/(m)
  }

  def valueOrArgument1[A](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A]) = valueOrParameters match {
    case \/-(r) => r.argument1(spec)
    case -\/(l) => a.apply(l)
  }

  def getBindedValue(key: Symbol): Option[SExpr] = bindings.getField(key).map(toSExpr)
  def getBindedValue(key: String): Option[SExpr] = bindings.getField(key).map(toSExpr)

  // lazy val functionParameters: List[SExpr] = value match {
  //   case SNil => Nil
  //   case m: SList => m.list.tail.map(_.resolve)
  //   case m => List(m.resolve)
  // }

  def toSExpr(p: Field): SExpr = config.toSExpr(p)
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
