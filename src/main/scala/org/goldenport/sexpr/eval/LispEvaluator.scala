package org.goldenport.sexpr.eval

import org.goldenport.sexpr._

/*
 * @since   Aug.  8, 2013
 * @version Aug.  9, 2013
 * @author  ASAMI, Tomoharu
 */
class LispEvaluator extends Evaluator {
  init_binding(new LispBinding)
}

class LispBinding extends Binding {
  def function(atom: SAtom): Option[List[SExpr] => SExpr] = {
    pf.lift(atom.name)
  }

  def pf: PartialFunction[String, List[SExpr] => SExpr] = {
    pf_Application orElse pf_lisp
  }

  protected def pf_Application: PartialFunction[String, List[SExpr] => SExpr] = Map.empty

  protected def pf_lisp: PartialFunction[String, List[SExpr] => SExpr] = {
    case "car" => func_car
    case "cdr" => func_cdr
    case "cons" => func_cons
    case "atom" => func_atom
    case "eq" => func_eq
    case "and" => func_and
    case "or" => func_or
  }

  protected def func_car(s: List[SExpr]): SExpr = {
    sys.error("???")
  }

  protected def func_cdr(s: List[SExpr]): SExpr = {
    sys.error("???")
  }

  protected def func_cons(s: List[SExpr]): SExpr = {
    sys.error("???")
  }

  protected def func_atom(s: List[SExpr]): SExpr = {
    sys.error("???")
  }

  protected def func_eq(s: List[SExpr]): SExpr = {
    sys.error("???")
  }

  protected def func_and(s: List[SExpr]): SExpr = {
    s match {
      case Nil => SBoolean.TRUE
      case _ if s.forall(SBoolean.isTrue) => s.last
      case _ => SBoolean.FALSE
    }
  }

  protected def func_or(s: List[SExpr]): SExpr = {
    s match {
      case Nil => SBoolean.FALSE
      case _ => s.find(SBoolean.isTrue) getOrElse SBoolean.FALSE
    }
  }
}
