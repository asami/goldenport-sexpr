package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import org.goldenport.exception.RAISE
import org.goldenport.log.LogMark, LogMark._
import org.goldenport.record.v3.Record
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.chart.ChartFunction
import org.goldenport.sexpr.eval.sql.SqlFunction
import org.goldenport.sexpr.eval.store.StoreFunction
import org.goldenport.sexpr.eval.entity.EntityFunction
import org.goldenport.sexpr.eval.repository.RepositoryFunction
import org.goldenport.sexpr.eval.projector.ProjectorFunction
import org.goldenport.sexpr.eval.camel.CamelFunction
import org.goldenport.sexpr.eval.aws.AwsFunction

/*
 * @since   Aug.  8, 2013
 *  version Feb. 27, 2014
 *  version Aug. 20, 2018
 *  version Sep. 29, 2018
 *  version Oct. 30, 2018
 *  version Feb. 28, 2019
 *  version Mar. 30, 2019
 *  version Apr. 14, 2019
 *  version May. 26, 2019
 * @version Jun.  1, 2019
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
    log_debug_start("apply", s"$c")
    val r = apply_context(c).resolve
    log_debug_end("apply", s"$c => $r")
    lift_Context(r)
  }

  def applyLazy(c: LispContext): LispContext = {
    log_debug_start("applyLazy", s"$c")
    val r = apply_context(c)
    log_debug_end("applyLazy", s"$c => $r")
    lift_Context(r)
  }

  protected def apply_context(c: LispContext): LispContext = {
    val ctx0 = lift_Context(c)
    val ctx1 = syntax_expansion(ctx0)
    val ctx = macro_expansion(ctx1)
    apply_lambda_option(ctx).orElse(
      get_function(lift_Context(ctx)).
        map(apply_function(ctx, _))
    ).getOrElse(eval_context(ctx))
  }

  protected def syntax_expansion(p: LispContext): LispContext = {
    val v = p.value
    val a = _quote_syntax(v)
    val b = _lambda_syntax(a)
    val r = b
    p.pure(r)
  }

  private def _quote_syntax(p: SExpr): SExpr = p match {
    case SCell(SSingleQuote(), m) => SList(SAtom.quote, m)
    case m => m
  }

  private def _lambda_syntax(p: SExpr): SExpr = p match {
    case SCell(SAtom.lambda, body) => SLambda.create(body)
    case SCell(SCell(SAtom.lambda, body), args) => SCell(SLambda.create(body), args)
    case m => m
  }

  protected def macro_expansion(c: LispContext): LispContext = c

  protected def apply_lambda_option(c: LispContext): Option[LispContext] = {
    c.value match {
      case m: SLambda => Some(c.toResult(m))
      case SCell(l @ SLambda(_, _), args) => Some(apply_lambda(c, l, args))
      case SCell(a @ SAtom(name), args) => c.getBindedValue(name).flatMap {
        case m: SLambda => Some(apply_lambda(c, m, args))
        case _ => None
      }
      case _ => None
    }
  }

  protected def apply_lambda(c: LispContext, l: SLambda, args: SExpr): LispContext = {
    args match {
      case m: SList => apply_lambda(c, l, m.list)
      case m => RAISE.syntaxErrorFault(l.toString)
    }
  }

  protected def apply_lambda(c: LispContext, l: SLambda, args: List[SExpr]): LispContext =
    apply_lambda_lexical_scope(c, l, args)

  protected def apply_lambda_lexical_scope(c: LispContext, l: SLambda, args: List[SExpr]): LispContext = {
    if (l.parameters.length > args.length)
      RAISE.syntaxErrorFault(s"""Missing argument for lambda($l): ${args.mkString(",")}""")
    apply_Lambda(c, l, args)
  }

  protected def apply_Lambda(c: LispContext, l: SLambda, args: List[SExpr]): LispContext = {
    val bindings = Record.create(l.parameters.zip(args))
    val ctx = c.addBindings(bindings)
    l.expressions./:(ctx)((z, x) => apply(ctx.pure(x)))
  }

  protected def apply_function(c: LispContext, f: LispFunction): LispContext = {
    c.log.trace(s"apply_function[${f.name}] ${c.value}")
    val r = try {
      f match {
        case m: EvalFunction =>
          c.log.trace(s"apply_function[Eval:${f.name}] ${c.value}")
          _eval_function(c, m)
        case m: ControlFunction =>
          c.log.trace(s"apply_function[Control:${f.name}] ${c.value}")
          m.apply(c)
        case m: AsyncIoFunction =>
          c.log.trace(s"apply_function[AsyncIoControl:${f.name}] ${c.value}")
          _eval_function(c, m)
        case m: IoFunction =>
          c.log.trace(s"apply_function[Io:${f.name}] ${c.value}")
          c.toResult(SFuture(c, m).start())
        case m: HeavyFunction =>
          c.log.trace(s"apply_function[Heavy:${f.name}] ${c.value}")
          c.toResult(SLazy(c, m))
        case m =>
          c.log.trace(s"apply_function[Misc:${f.name}] ${c.value}")
          _eval_function(c, m)
      }
    } catch {
      case NonFatal(e) =>
        val label = f.specification.label(e)
        c.toResult(SError(label, e))
    }
    c.log.trace(s"apply_function[${f.name}] ${c.value} => $r")
    r
  }

  private def _eval_function(c: LispContext, f: LispFunction): LispContext = {
    val a = c.reductForEval
    if (_is_lazy(a.value))
      c.toResult(SLazy(c, f)) // lazy evaluation for SControl(e.g. SFuture) elements.
    else
      f(a)
  }

  private def _is_lazy(p: SExpr) = p.getList.exists(_.isInstanceOf[SControl])

  protected def eval_context(c: LispContext): LispContext = {
    // println(s"LispEvaluator#evel_context: ${c.bindings}")
    // println(s"LispEvaluator#evel_context: ${c.value}")
    val r: SExpr = c.value match {
      case SAtom(name) => c.getBindedValue(name).getOrElse(SError.bindingNotFound(name))
      case m @ SCell(car, _) if car.isInstanceOf[SCell] => SError.Unevaluatable(m)
      // case m: SCell => eval(m)
      // case m => m
      case m => eval(m)
    }
    c.toResult(r)
  }

  override protected def eval_list_to_context(list: SList): C = list match {
    case SNil => create_Eval_Context(SNil)
    case m: SCell =>
      val ctx = create_Eval_Context(m)
      lift_Context(apply_context(ctx))
      // get_function(ctx).
      //   map(x => lift_Context(apply_function(ctx, x))).
      //   getOrElse(RAISE.syntaxErrorFault(list.print))
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

  def useSql: Boolean = true
  def useStore: Boolean = true
  def useEntity: Boolean = true
  def useRepository: Boolean = true
  def useChart: Boolean = true
  def useCamel: Boolean = true
  def useAws: Boolean = true

  private lazy val _functions: Vector[LispFunction] = {
    import LispFunction._
    Vector(
      EvalOrInvoke, Quote, Setq,
      Pop, Peek, Mute, History, CommandHistory,
      Car, Cdr, And, Or, Plus, Length, PathGet, Transform,
      Fetch, Retry, Sh,
      HttpGet, HttpPost, HttpPut, HttpDelete,
      MatrixLoad, MatrixChart,
      TableLoad, TableMake, TableChart
    ) ++ EmacsLispFunction.functions ++ SchemeFunction.functions ++
    _sql_functions ++ _store_functions ++ _entity_functions ++ _repository_functions ++
    _chart_functions ++
    _projector_functions ++ _camel_functions ++ _aws_functions
  }

  private def _sql_functions = 
    if (useSql)
      SqlFunction.functions
    else
      Vector.empty

  private def _store_functions = 
    if (useStore)
      StoreFunction.functions
    else
      Vector.empty

  private def _entity_functions = 
    if (useEntity)
      EntityFunction.functions
    else
      Vector.empty

  private def _repository_functions = 
    if (useRepository)
      RepositoryFunction.functions
    else
      Vector.empty

  private def _chart_functions = 
    if (useChart)
      ChartFunction.functions
    else
      Vector.empty

  private def _projector_functions = ProjectorFunction.functions

  private def _camel_functions = 
    if (useCamel)
      CamelFunction.functions
    else
      Vector.empty

  private def _aws_functions = 
    if (useAws)
      AwsFunction.functions
    else
      Vector.empty

  protected def functions_Pf: PartialFunction[C,LispFunction] = Map.empty

  // private val _control_functions = Vector("and", "or")

  // override protected def is_Control_Function(p: SAtom) =
  //   Some(_control_functions.contains(p.name))

  override def getFunction(c: C): Option[LispFunction] =
    _functions.toStream.filter(_.isDefinedAt(c)).headOption.orElse(
      functions_Pf.lift(c)).
      orElse(quote_function(c)). // See _quote_syntax
      orElse(dynamic_service_function(c))

  protected def quote_function(c: C): Option[LispFunction] =
    c.value match {
      case SCell(SSingleQuote(), _) =>
        Some(LispFunction.Quote)
      case _ => None
    }

  protected def dynamic_service_function(c: C): Option[LispFunction] = {
    c.value match {
      case SCell(SAtom(name), _) =>
        if (c.isUnavailableFunction(name))
          None
        else
          Some(c.createDynamicServiceFunction(name))
      case _ => None
    }
  }

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
  def initialize() {
    import play.api.libs.json.JsValue
    import org.apache.commons.jxpath.JXPathIntrospector
    import org.goldenport.json.JsonDynamicPropertyHandler
    import org.goldenport.extension._

    JXPathIntrospector.registerDynamicClass(classOf[IRecord], classOf[RecordDynamicPropertyHandler])
    JXPathIntrospector.registerDynamicClass(classOf[JsValue], classOf[JsonDynamicPropertyHandler])
  }

  def apply(): LispBinding[LispContext] = new LispBinding[LispContext]() {
  }
}
