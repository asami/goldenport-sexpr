package org.goldenport.sexpr.eval

import scala.collection.mutable.{
  Stack, HashMap
}
import org.goldenport.sexpr._

/*
 * @since   Aug.  8, 2013
 *  version Dec.  9, 2013
 * @version Feb.  4, 2014
 * @author  ASAMI, Tomoharu
 */
trait Evaluator {
  private val _stack = new Stack[Binding]

  protected def init_binding(binding: Binding) {
    _stack.push(binding)
  }

  def eval(in: CharSequence): SExpr = {
    eval(SExprParser(in))
  }

  def eval(expr: SExpr): SExpr = {
    // println("Evaluator#expr = " + expr)
    expr match {
      case atom: SAtom => eval_atom(atom)
      case keyword: SKeyword => eval_keyword(keyword)
      case num: SNumber => eval_number(num)
      case b: SBoolean => eval_boolean(b)
      case s: SString => eval_string(s)
      case xs: SList => eval_list(xs)
      case p: SPseudo => eval_pseudo(p)
    }
  }

  protected def eval_atom(atom: SAtom): SExpr = {
    if (_stack.isEmpty) throw new IllegalStateException("Stack should be pushed init binding.")
    _stack.toStream.flatMap(_.get(atom)).headOption match {
      case Some(s) => s
      case None => sys.error("???")
    }
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
        _stack.toStream.flatMap(_.function(atom)).headOption match {
          case Some(f) => f(c)
          case None => sys.error("???")
        }
      }
      case _ => sys.error("???")
    }
  }

  protected def eval_pseudo(pseudo: SPseudo): SExpr = {
    sys.error("???")
  }
}

trait Binding {
  val binds = new HashMap[String, SExpr]

  def get(atom: SAtom): Option[SExpr] = {
    binds.get(atom.name) orElse get_Application(atom.name)
  }

  protected def get_Application(name: String): Option[SExpr] = None

  def function(atom: SAtom): Option[List[SExpr] => SExpr]
}
