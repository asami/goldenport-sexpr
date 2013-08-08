package org.goldenport.sexpr.eval

import org.goldenport.sexpr._

/*
 * @since   Aug.  8, 2013
 * @version Aug.  8, 2013
 * @author  ASAMI, Tomoharu
 */
trait Evaluator {
  def binding: Binding

  def eval(in: CharSequence): SExpr = {
    SExprParser(in)
  }

  def eval(expr: SExpr): SExpr = {
    expr match {
      case atom: SAtom => eval_atom(atom)
      case keyword: SKeyword => eval_keyword(keyword)
      case num: SNumber => eval_number(num)
      case b: SBoolean => eval_boolean(b)
      case s: SString => eval_string(s)
      case xs: SList => eval_list(xs)
    }
  }

  protected def eval_atom(atom: SAtom): SExpr = {
    binding.get(atom)
  }

  protected def eval_keyword(keyword: SKeyword): SExpr = {
    sys.error("???")
  }

  protected def eval_number(num: SNumber): SExpr = {
    num
  }

  protected def eval_boolean(b: SBoolean): SExpr = {
    b
  }

  protected def eval_string(s: SString): SExpr = {
    s
  }

  protected def eval_list(xs: SList): SExpr = {
    val b = xs.list
    b.head match {
      case atom: SAtom => {
        val c = b.tail.map(eval)
        binding.function(atom)(c)
      }
      case _ => sys.error("???")
    }
  }
}

trait Binding {
  def get(atom: SAtom): SExpr
  def function(atom: SAtom)(xs: List[SExpr]): SExpr
}
