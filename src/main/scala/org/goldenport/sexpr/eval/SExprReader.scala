package org.goldenport.sexpr.eval

import org.goldenport.sexpr._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

/*
 * @since   Jan.  9, 2014
 * @version Jan.  9, 2014
 * @author  ASAMI, Tomoharu
 */
case class SExprReader(exprs: Seq[SExpr]) extends Reader[SExpr] {
  def first = exprs.head
  def rest = SExprReader(exprs.tail)
  def atEnd = exprs.isEmpty
  def pos = NoPosition
}

object SExprReader {
  def create(expr: SExpr) = {
    val exprs: Seq[SExpr] = create_stream(expr)
    new SExprReader(exprs)
  }

  protected def create_stream(expr: SExpr): Stream[SExpr] = {
    expr match {
      case x: SAtom => Stream(x)
      case x: SKeyword => Stream(x)
      case x: SNumber => Stream(x)
      case x: SString => Stream(x)
      case x: SBoolean => Stream(x)
      case SNil => Stream(SNil)
      case x @ SCell(car, cdr: SList) => {
        val xs = x.list.toStream
        SOpen #:: xs.flatMap(create_stream) ++ Stream(SClose)
      }
      case x: SCell => Stream(x)
    }
  }
}
