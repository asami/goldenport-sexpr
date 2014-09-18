package org.goldenport.sexpr.eval

import scala.collection.mutable.{
  Stack, HashMap
}
import org.goldenport.sexpr._

/*
 * @since   Aug.  8, 2013
 *  version Dec.  9, 2013
 *  version Feb. 28, 2014
 *  version Mar. 11, 2014
 *  version Aug. 14, 2014
 * @version Sep. 18, 2014
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
      case None => {
        val cs = Vector(create_Eval_Context(Nil))
        _stack.toStream.flatMap(_.function(atom)).headOption match {
          case Some(f) => f(reduction_Context(cs)).value
          case None => sys.error("???")
        }
      }
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
    eval_list_to_context(xs).value
  }

  protected def eval_pseudo(pseudo: SPseudo): SExpr = {
    sys.error("???")
  }

/*
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
*/

  protected def eval_to_context(expr: SExpr): EvalContext = {
    expr match {
      case atom: SAtom => eval_atom_to_context(atom)
      case keyword: SKeyword => eval_keyword_to_context(keyword)
      case num: SNumber => eval_number_to_context(num)
      case b: SBoolean => eval_boolean_to_context(b)
      case s: SString => eval_string_to_context(s)
      case xs: SList => eval_list_to_context(xs)
      case p: SPseudo => eval_pseudo_to_context(p)
    }
  }

  protected def eval_atom_to_context(atom: SAtom): EvalContext = {
    create_Eval_Context(eval_atom(atom))
  }

  protected def eval_keyword_to_context(keyword: SKeyword): EvalContext = {
    create_Eval_Context(eval_keyword(keyword))
  }

  protected def eval_number_to_context(number: SNumber): EvalContext = {
    create_Eval_Context(eval_number(number))
  }

  protected def eval_boolean_to_context(boolean: SBoolean): EvalContext = {
    create_Eval_Context(eval_boolean(boolean))
  }

  protected def eval_string_to_context(string: SString): EvalContext = {
    create_Eval_Context(eval_string(string))
  }

  protected def eval_pseudo_to_context(pseuedo: SPseudo): EvalContext = {
    create_Eval_Context(eval_pseudo(pseuedo))
  }

  protected def eval_list_to_context(list: SList): EvalContext = {
    val b = list.list
    b.head match {
      case atom: SAtom => {
        val cs = b.tail.map(eval_to_context)
        _stack.toStream.flatMap(_.function(atom)).headOption match {
          case Some(f) => f(reduction_Context(cs))
          case None => sys.error("???")
        }
      }
      case _ => sys.error("???")
    }
  }

  protected def create_Eval_Context(x: SExpr): EvalContext

  protected def create_Eval_Context(xs: List[SExpr]): EvalContext

  protected def reduction_Context(xs: Seq[EvalContext]): EvalContext

  // parameter
  // TODO unify SExprConverters
  protected def parameter_strings(xs: Seq[SExpr]): Seq[String] = {
    xs map {
      case SAtom(s) => s
      case SString(s) => s
      case x => throw new IllegalArgumentException(s"No atom or string = $x")
    }
  }

  protected def parameter_boolean(xs: Seq[SExpr], name: String): Boolean = {
    val NAME = name
    xs.exists {
      case SAtom(NAME) => true
      case _ => false
    }
  }

  protected def parameter_boolean_opt(xs: Seq[SExpr], name: String): Option[Boolean] = {
    val NAME = name
    val a = xs collect {
      case SAtom(NAME) => Some(true)
    }
    if (a.contains(Some(true))) Some(true)
    else if (a.nonEmpty) Some(false)
    else None
  }

  protected def parameter_bytes(xs: Seq[SExpr]): Seq[Byte] = {
    xs map {
      case SNumber(s) => s.toByte
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  protected def parameter_shorts(xs: Seq[SExpr]): Seq[Short] = {
    xs map {
      case SNumber(s) => s.toShort
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  protected def parameter_ints(xs: Seq[SExpr]): Seq[Int] = {
    xs map {
      case SNumber(s) => s.toInt
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  protected def parameter_longs(xs: Seq[SExpr]): Seq[Long] = {
    xs map {
      case SNumber(s) => s.toLong
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  protected def parameter_floats(xs: Seq[SExpr]): Seq[Float] = {
    xs map {
      case SNumber(s) => s.toFloat
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  protected def parameter_doubles(xs: Seq[SExpr]): Seq[Double] = {
    xs map {
      case SNumber(s) => s.toDouble
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  protected def parameter_values[T](master: Map[String, T], xs: Seq[SExpr]): Seq[T] = {
    strings_to_values(master, parameter_strings(xs))
  }

  protected def strings_to_values[T](master: Map[String, T], xs: Seq[String]): Seq[T] = {
    xs map { x =>
      master.get(x) getOrElse {
        throw new IllegalArgumentException("Illegal symbol '$x', Available symbols are: ${master.keys.mkString(\", \")}")
      }
    }
  }

  protected def to_boolean(v: Boolean) = {
    if (v) SBoolean.TRUE
    else SBoolean.FALSE
  }
}

trait Binding {
  val binds = new HashMap[String, SExpr]

  def get(atom: SAtom): Option[SExpr] = {
    binds.get(atom.name) orElse eval_Atom(atom.name)
  }

  protected def eval_Atom(name: String): Option[SExpr] = None

  def function(atom: SAtom): Option[EvalContext => EvalContext]
}
