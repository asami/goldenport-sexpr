package org.goldenport.sexpr.eval

import scala.collection.mutable.{
  Stack, HashMap
}
import org.goldenport.sexpr._

/*
 * @since   Aug.  9, 2013
 * @version Feb.  4, 2014
 * @author  ASAMI, Tomoharu
 */
trait Translator[T] {
  def translate(expr: SExpr): T = {
    expr match {
      case atom: SAtom => trans_atom(atom)
      case keyword: SKeyword => trans_keyword(keyword)
      case num: SNumber => trans_number(num)
      case b: SBoolean => trans_boolean(b)
      case s: SString => trans_string(s)
      case xs: SList => trans_list(xs)
      case p: SPseudo => trans_pseudo(p)
    }
  }

  protected def trans_function(name: String, xs: List[T]): T = {
    pf(name)(xs)
  }
  protected def trans_atom(atom: SAtom): T
  protected def trans_keyword(keyword: SKeyword): T
  protected def trans_number(num: SNumber): T
  protected def trans_boolean(b: SBoolean): T
  protected def trans_string(s: SString): T
  protected def trans_list(xs: SList): T = {
    val b = xs.list
    b.head match {
      case atom: SAtom => {
        val c = b.tail.map(translate)
        trans_function(atom.name, c)
      }
      case _ => sys.error("???")
    }
  }
  protected def trans_pseudo(p: SPseudo): T = ???

  protected def pf: PartialFunction[String, List[T] => T]
}
