package org.goldenport.sexpr.eval

import org.goldenport.exception.RAISE
import org.goldenport.log.LogMark, LogMark._
import org.goldenport.record.v3.Record
import org.goldenport.sexpr._

/*
 * @since   Aug.  8, 2013
 *  version Feb. 27, 2014
 *  version Aug. 20, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
trait LispEvaluator[C <: LispContext] extends Evaluator[C]
    with JXPathPart[C] {
  def config: LispConfig
  protected def create_Eval_Context(): C = create_Eval_Context(SNil)
  protected def create_Eval_Context(x: SExpr): C = LispContext(config, apply_context, x).asInstanceOf[C]
  protected def create_Eval_Context(xs: List[SExpr]): C = create_Eval_Context(SList.create(xs))
  protected def reduction_Context(xs: Seq[C]): C = create_Eval_Context(xs.toList.map(_.value))
  protected def lift_Context(c: EvalContext): C = c.asInstanceOf[C]

  // def pure(p: SExpr) = create_Eval_Context(p)

  def apply(c: LispContext): LispContext = {
    log.debug(ExecuteLocation, StartAction, "apply", s"$c")
    val r = apply_context(c).resolve
    log.debug(ExecuteLocation, EndAction, "apply", s"$c => $r")
    lift_Context(r)
  }

  def applyLazy(c: LispContext): LispContext = {
    log.debug(ExecuteLocation, StartAction, "applyLazy", s"$c")
    val r = apply_context(c)
    log.debug(ExecuteLocation, EndAction, "applyLazy", s"$c => $r")
    lift_Context(r)
  }

  protected def apply_context(c: LispContext): LispContext =
    get_function(lift_Context(c)).
      map(apply_function(c, _)).
      getOrElse(eval_context(c))

  protected def apply_function(c: LispContext, f: LispFunction): LispContext = {
    c.log.trace(s"apply_functionx(${f.name}: ${c.value}")
    val r = f match {
      case m: EvalFunction => _eval_function(c, m)
      case m: ControlFunction => m.apply(c)
      case m: IoFunction => c.toResult(SFuture(c, m).start())
      case m: HeavyFunction => c.toResult(SLazy(c, m))
      case m => _eval_function(c, m)
    }
    c.log.trace(s"apply_functionx(${f.name}: ${c.value}")
    r
  }

  private def _eval_function(c: LispContext, f: LispFunction): LispContext = {
    val a = c.reductForEval
    if (_is_lazy(a.value))
      c.toResult(SLazy(c, f))
    else
      f(a)
  }

  private def _is_lazy(p: SExpr) = p.getList.exists(_.isInstanceOf[SControl])

  protected def eval_context(c: LispContext): LispContext = c.toResult(eval(c.value))

  override protected def eval_list_to_context(list: SList): C = list match {
    case SNil => create_Eval_Context(SNil)
    case m: SCell =>
      val ctx = create_Eval_Context(m)
      get_function(ctx).
        map(x => lift_Context(apply_function(ctx, x))).
        getOrElse(RAISE.syntaxErrorFault(list.print))
    case m => create_Eval_Context(m)
  }

  override protected def eval_script_to_context(p: SScript): C = {
    val ctx = create_Eval_Context()
    lift_Context(ctx.script.eval(p))
  }
}
object LispEvaluator {
  def apply(p: LispConfig): LispEvaluator[LispContext] = new LispEvaluator[LispContext]() {
    val config = p
    init_binding(LispBinding())
  }
}

trait LispBinding[C <: LispContext] extends Binding[C] {
  binds.put("t", SBoolean.TRUE)
  binds.put("nil", SNil)

  private lazy val _functions: Vector[LispFunction] = {
    import EvalFunction._
    Vector(Car, And, Or, Plus, Length, Pop, Fetch, HttpGet)
  }

  // private val _control_functions = Vector("and", "or")

  // override protected def is_Control_Function(p: SAtom) =
  //   Some(_control_functions.contains(p.name))

  override def getFunction(c: C): Option[LispFunction] =
    _functions.toStream.filter(_.isDefinedAt(c)).headOption

  def getFunction(p: String): Option[LispFunction] =
    _functions.toStream.find(_.name == p)

  def function(atom: SAtom): Option[C => C] = None

  // def function(atom: SAtom): Option[C => C] = {
  //   pf.lift(atom.name)
  // }

  // def pf: PartialFunction[String, C => C] = {
  //   pf_Application orElse pf_lisp
  // }

  // protected def pf_Application: PartialFunction[String, C => C] = Map.empty

  // protected def pf_lisp: PartialFunction[String, C => C] = {
  //   case "car" => func_car
  //   case "cdr" => func_cdr
  //   case "cons" => func_cons
  //   case "atom" => func_atom
  //   case "eq" => func_eq
  //   case "and" => func_and
  //   case "or" => func_or
  //   case "+" => func_plus
  // }

  // protected def func_car(p: C): C = {
  //   sys.error("???")
  // }

  // protected def func_cdr(p: C): C = {
  //   sys.error("???")
  // }

  // protected def func_cons(p: C): C = {
  //   sys.error("???")
  // }

  // protected def func_atom(p: C): C = {
  //   sys.error("???")
  // }

  // protected def func_eq(p: C): C = {
  //   sys.error("???")
  // }

  // protected final def exec_args(p: C)(body: List[SExpr] => SExpr): C = {
  //   val r = body(p.args)
  //   // println(s"exec_args($p): $r")
  //   p.toResult(r).asInstanceOf[C]
  // }

  // protected final def func_and(p: C): C = exec_args(p)(_ match {
  //   case Nil => SBoolean.TRUE
  //   case m if m.forall(SBoolean.isTrue) => SBoolean.TRUE
  //   case _ => SBoolean.FALSE
  // })

  // protected final def func_or(p: C): C = exec_args(p)(_ match {
  //   case Nil => SBoolean.FALSE
  //   case m => m.find(SBoolean.isTrue) getOrElse SBoolean.FALSE
  // })

  // protected final def func_plus(p: C): C = exec_args(p)(_ match {
  //   case Nil => SNumber.zero
  //   case m => SNumber(m.map(_.asNumber).map(_.number).sum)
  // })
}
object LispBinding {
  def apply(): LispBinding[LispContext] = new LispBinding[LispContext]() {
  }
}
