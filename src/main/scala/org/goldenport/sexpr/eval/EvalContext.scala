package org.goldenport.sexpr.eval

import org.goldenport.sexpr._

/*
 * @since   Feb. 27, 2014
 * @version Sep. 18, 2014
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

  def arg: SExpr = {
    value match {
      case xs: SList => xs.list.head
      case _ => value
    }
  }

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
}
