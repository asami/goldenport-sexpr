package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import scala.collection.mutable.{Stack, HashMap}
import org.goldenport.log.{LogMark, Loggable}, LogMark._
import org.goldenport.recorder.Recordable
import org.goldenport.parser.CommandParser
import org.goldenport.sexpr._

/*
 * @since   Aug.  8, 2013
 *  version Dec.  9, 2013
 *  version Feb. 28, 2014
 *  version Mar. 11, 2014
 *  version Aug. 14, 2014
 *  version Sep. 18, 2014
 *  version Oct.  8, 2014
 *  version Jun. 17, 2015
 *  version Aug. 19, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Feb. 28, 2019
 *  version Mar. 10, 2019
 *  version Apr. 20, 2019
 *  version May.  3, 2019
 *  version Jul. 25, 2019
 *  version Aug. 31, 2019
 *  version Sep. 16, 2019
 *  version Oct. 31, 2019
 *  version Jan. 24, 2021
 *  version Mar. 21, 2021
 * @version Aug.  4, 2023
 * @author  ASAMI, Tomoharu
 */
trait Evaluator[C <: EvalContext] extends Loggable with Recordable {
  override protected def log_Location = InterpreterLocation
  override protected def log_Label = Some("evaluator")

  private val _stack = new Stack[Binding[C]]

  protected def init_binding(binding: Binding[C]) {
    _stack.push(binding)
  }

  protected final def get_function[T](ctx: C): Option[LispFunction] = 
    _stack.toStream.flatMap(_.getFunction(ctx)).headOption

  protected final def get_Specification[T](name: String): Option[FunctionSpecification] = 
    _stack.toStream.flatMap(_.getSpecification(name)).headOption

  protected final def get_binded_value(atom: SAtom): Option[SExpr] =
    _stack.toStream.flatMap(_.get(atom)).headOption

  protected final def get_macro[T](ctx: C): Option[LispMacro] = None // TODO

  lazy val functionParser: CommandParser[LispFunction] =
    _stack.map(_.functionParser)./:(CommandParser.empty[LispFunction]) { (z, x) =>
      z append x
    }

  def parse(in: CharSequence): SExpr = SExprParserNew(in)

  def apply(expr: SExpr): C = {
    log_debug_start("evaluator-apply", s"evaluator-apply: $expr")
    // val a = eval_to_context(expr)
    // println(s"Evaluator#apply $a")
    val r: EvalContext = eval_to_context(expr).resolve
    // println(s"evaluator-apply: $expr => $r")
    // println(s"evaluator r.resolve: ${r.resolve}")
    log_debug_end("evaluator-apply", s"evaluator-apply: $expr => $r")
    lift_Eval_Context(r)
  }

  def applyLazy(expr: SExpr): C = {
    log_debug_start("evaluator-applylazy", s"evaluator-applylazy: $expr")
    val r: EvalContext = eval_to_context(expr)
    log_debug_end("evaluator-applylazy", s"evaluator-applylazy: $expr => $r")
    lift_Eval_Context(r)
  }

  def eval(in: CharSequence): SExpr = {
    eval(parse(in))
  }

  def eval(expr: SExpr): SExpr = {
    log_debug_start("evaluator-eval", s"evaluator-eval: $expr")
    val a = expr match {
      case atom: SAtom => eval_atom(atom)
      case keyword: SKeyword => eval_keyword(keyword)
      case num: SNumber => eval_number(num)
      case m: SRational => m
      case m: SComplex => m
      case b: SBoolean => eval_boolean(b)
      case s: SString => eval_string(s)
      case xs: SList => eval_list(xs)
      case m: SError => m
      case m: SBinary => m
      case m: SI18NString => m
      case m: SI18NTemplate => m
      case m: SRegex => m
      case m: SDocument => m
      case m: SModel => m
      case r: SRecord => eval_record(r)
      case t: STable => eval_table(t)
      case m: SMatrix => m
      case m: SLxsv => m
      case m: STree => m
      case m: SUrl => m
      case m: SUrn => m
      case m: SScript => eval_script(m)
      case m: SObject => m
      case x: SXml => eval_xml(x)
      case m: SHtml => m
      case m: SXPath => m
      case m: SXsl => m
      case m: SPug => m
      case j: SJson => eval_json(j)
      case m: SExpression => eval_expression(m)
      case m: SExtension => eval_extension(m)
      case p: SPseudo => eval_pseudo(p)
      case m => m
    }
    val r = a.resolve
    log_debug_end("evaluator-eval", s"evaluator-eval: $expr => $r")
    r
  }

  protected def eval_atom(atom: SAtom): SExpr = {
    if (_stack.isEmpty)
      throw new IllegalStateException("Stack should be pushed init binding.")
    log_trace(s"Evaluator:eval_atom($atom): ${_stack}")
    eval_Atom(atom) getOrElse _eval_atom(atom)
  }

  protected def eval_Atom(atom: SAtom): Option[SExpr] = None

  private def _eval_atom(atom: SAtom): SExpr = eval_atom_option(atom).
    getOrElse(SError.bindingNotFound(atom.name))

  protected final def eval_atom_option(atom: SAtom): Option[SExpr] =
    get_binded_value(atom).orElse {
      val cs = Vector(create_Eval_Context(Nil))
      _stack.toStream.flatMap(_.function(atom)).headOption match {
        case Some(f) => Some(f(reduction_Context(cs)).value)
        case None => None
      }
    }

  protected def eval_keyword(keyword: SKeyword): SExpr = keyword

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

  protected def eval_record(p: SRecord): SExpr = p

  protected def eval_table(p: STable): SExpr = p

  protected def eval_matrix(p: SMatrix): SExpr = p

  protected def eval_lxsv(p: SLxsv): SExpr = p

  protected def eval_tree(p: STree): SExpr = p

  protected def eval_document(p: SDocument): SExpr = p

  protected def eval_model(p: SModel): SExpr = p

  protected def eval_datetime(p: SDateTime): SExpr = p

  protected def eval_localdate(p: SLocalDate): SExpr = p

  protected def eval_localtime(p: SLocalTime): SExpr = p

  protected def eval_url(p: SUrl): SExpr = p

  protected def eval_urn(p: SUrn): SExpr = p

  protected def eval_script(p: SScript): SExpr = p

  protected def eval_object(p: SObject): SExpr = p

  protected def eval_xml(p: SXml): SExpr = p

  protected def eval_json(p: SJson): SExpr = p

  protected def eval_xpath(p: SXPath): SExpr = p

  protected def eval_expression(p: SExpression): SExpr = {
    val atom = SAtom(p.expression)
    eval_atom_option(atom).
      getOrElse(SError.bindingNotFound(p.expression))
  }

  protected def eval_extension(p: SExtension): SExpr = p

  protected def eval_pseudo(pseudo: SPseudo): SExpr = {
    sys.error(s"Evaluator#eval_pseudo: $pseudo")
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

  protected def eval_to_context(expr: SExpr): C = try {
    expr match {
      case atom: SAtom => eval_atom_to_context(atom)
      case keyword: SKeyword => eval_keyword_to_context(keyword)
      case num: SNumber => eval_number_to_context(num)
      case b: SBoolean => eval_boolean_to_context(b)
      case s: SString => eval_string_to_context(s)
      case xs: SList => eval_list_to_context(xs)
      case r: SRecord => eval_record_to_context(r)
      case t: STable => eval_table_to_context(t)
      case m: SMatrix => eval_matrix_to_context(m)
      case m: SLxsv => eval_lxsv_to_context(m)
      case m: STree => eval_tree_to_context(m)
      case m: SDocument => eval_document_to_context(m)
      case m: SModel => eval_model_to_context(m)
      case m: SDateTime => eval_datetime_to_context(m)
      case m: SLocalDate => eval_localdate_to_context(m)
      case m: SLocalTime => eval_localtime_to_context(m)
      case m: SUrl => eval_url_to_context(m)
      case m: SUrn => eval_urn_to_context(m)
      case m: SScript => eval_script_to_context(m)
      case m: SObject => eval_object_to_context(m)
      case x: SXml => eval_xml_to_context(x)
      case j: SJson => eval_json_to_context(j)
      case m: SXPath => eval_xpath_to_context(m)
      case m: SExpression => eval_expression_to_context(m)
      case m: SExtension => eval_extension_to_context(m)
      case p: SPseudo => eval_pseudo_to_context(p)
      case m => create_Eval_Context(m)
    }
  } catch {
    case NonFatal(e) => create_Eval_Context(SError(e))
  }

  protected def eval_atom_to_context(atom: SAtom): C = {
    create_Eval_Context(eval_atom(atom))
  }

  protected def eval_keyword_to_context(keyword: SKeyword): C = {
    create_Eval_Context(eval_keyword(keyword))
  }

  protected def eval_number_to_context(number: SNumber): C = {
    create_Eval_Context(eval_number(number))
  }

  protected def eval_boolean_to_context(boolean: SBoolean): C = {
    create_Eval_Context(eval_boolean(boolean))
  }

  protected def eval_string_to_context(string: SString): C = {
    create_Eval_Context(eval_string(string))
  }

  protected def eval_record_to_context(p: SRecord): C = {
    create_Eval_Context(eval_record(p))
  }

  protected def eval_table_to_context(p: STable): C = {
    create_Eval_Context(eval_table(p))
  }

  protected def eval_matrix_to_context(p: SMatrix): C = {
    create_Eval_Context(eval_matrix(p))
  }

  protected def eval_lxsv_to_context(p: SLxsv): C = {
    create_Eval_Context(eval_lxsv(p))
  }

  protected def eval_tree_to_context(p: STree): C = {
    create_Eval_Context(eval_tree(p))
  }

  protected def eval_document_to_context(p: SDocument): C = {
    create_Eval_Context(eval_document(p))
  }

  protected def eval_model_to_context(p: SModel): C = {
    create_Eval_Context(eval_model(p))
  }

  protected def eval_datetime_to_context(p: SDateTime): C = {
    create_Eval_Context(eval_datetime(p))
  }

  protected def eval_localdate_to_context(p: SLocalDate): C = {
    create_Eval_Context(eval_localdate(p))
  }

  protected def eval_localtime_to_context(p: SLocalTime): C = {
    create_Eval_Context(eval_localtime(p))
  }

  protected def eval_url_to_context(p: SUrl): C = {
    create_Eval_Context(eval_url(p))
  }

  protected def eval_urn_to_context(p: SUrn): C = {
    create_Eval_Context(eval_urn(p))
  }

  // override by LispEvaluator
  protected def eval_script_to_context(p: SScript): C = {
    create_Eval_Context(eval_script(p))
  }

  protected def eval_object_to_context(p: SObject): C = {
    create_Eval_Context(eval_object(p))
  }

  protected def eval_xml_to_context(p: SXml): C = {
    create_Eval_Context(eval_xml(p))
  }

  protected def eval_json_to_context(p: SJson): C = {
    create_Eval_Context(eval_json(p))
  }

  protected def eval_xpath_to_context(p: SXPath): C = {
    val r = create_Eval_Context(eval_xpath(p))
    // println("X: $r")
    r
  }

  // override by LispEvaluator
  protected def eval_expression_to_context(p: SExpression): C = {
    create_Eval_Context(eval_expression(p))
  }

  protected def eval_extension_to_context(p: SExtension): C = {
    create_Eval_Context(eval_extension(p))
  }

  protected def eval_pseudo_to_context(pseuedo: SPseudo): C = {
    create_Eval_Context(eval_pseudo(pseuedo))
  }

  protected def eval_list_to_context(list: SList): C = list match {
    case SNil => create_Eval_Context(SNil)
    case SCell(car, cdr) => car match {
      case atom: SAtom => cdr match {
        case m: SList =>
          _stack.toStream.flatMap(_.function(atom)).headOption match {
            case Some(f) =>
              if (is_control_function(atom))
                _apply_function(f, m)
              else
                _eval_function(f, m)
            case None =>
              log_trace(s"No function: $atom") // TODO
              log_trace(s"Stack: ${_stack}") // TODO
              create_Eval_Context(list)
          }
        case m => throw new SyntaxErrorException(s"No list prameter: $m")
      }
      case m => throw new SyntaxErrorException(s"No atom: $m")
    }
  }

  protected final def is_control_function(p: SAtom): Boolean = (
    _stack.toStream.flatMap(_.isControlFunction(p)).headOption.getOrElse(false) ||
      is_Control_Function(p)
  )

  protected def is_Control_Function(p: SAtom): Boolean = false

  private def _eval_function(f: C => C, p: SList): C = {
    log_trace_start(s"_eval_function: $p")
    val cs = p.list.map(eval_to_context)
    val r = f(reduction_Context(cs))
    log_trace_end(s"_eval_function: $p => $r")
    r
  }

  private def _apply_function(f: C => C, p: SList): C = {
    log_trace_start(s"_apply_function: $p")
    val r = f(create_Eval_Context(p))
    log_trace_end(s"_apply_function: $p => $r")
    r
  }

  protected def lift_Eval_Context(p: EvalContext): C = p.asInstanceOf[C]

  protected def create_Eval_Context(x: SExpr): C

  protected def create_Eval_Context(xs: List[SExpr]): C

  protected def reduction_Context(xs: Seq[C]): C

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

  /*
   * Result value conversion
   */
  // deprecated
  protected def to_boolean(v: Boolean) = {
    if (v) SBoolean.TRUE
    else SBoolean.FALSE
  }

  protected def to_sboolean(v: Boolean) = {
    if (v) SBoolean.TRUE
    else SBoolean.FALSE
  }

  protected def to_sstring(v: String) = SString(v)

  protected def to_sstrings(v: Seq[String]) = {
    SList.create(v.map(to_sstring))
  }

  protected def to_sstringmap(v: Map[String, String]) = {
    val a = v.toList.map(x => SCell(SString(x._1), SString(x._2)))
    SList.create(a)
  }

  /*
   * Utility
   */
  protected def is_true(expr: SExpr): Boolean = {
    expr match {
      case SBoolean(v) => v
      case SNil => false
      case _ => true
    }
  }
}

trait Binding[C <: EvalContext] {
  val binds = new HashMap[String, SExpr]

  def get(atom: SAtom): Option[SExpr] = {
    binds.get(atom.name) orElse get_Atom(atom.name)
  }

  protected def get_Atom(name: String): Option[SExpr] = None

  def function(atom: SAtom): Option[C => C]

  def isControlFunction(atom: SAtom): Option[Boolean] = is_Control_Function(atom)

  protected def is_Control_Function(atom: SAtom): Option[Boolean] = None

  def getFunction(ctx: C): Option[LispFunction] = None

  def getSpecification(name: String): Option[FunctionSpecification] = None

  def functionParser: CommandParser[LispFunction]
}
