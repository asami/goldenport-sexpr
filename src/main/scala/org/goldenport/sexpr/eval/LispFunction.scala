package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import java.io.File
import java.nio.charset.Charset
import java.net.{URL, URI}
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.i18n.I18NMessage
import org.goldenport.context.Effect
import org.goldenport.context.Consequence
import org.goldenport.record.v2.{XInt, XDecimal}
import org.goldenport.record.v3.{Record, ITable}
import org.goldenport.record.v3.Table.CreateHtmlStrategy
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.http.{Request, Response}
import org.goldenport.io.{MimeType, UrlUtils, Retry => LibRetry}
import org.goldenport.io.ResourceHandle
import org.goldenport.matrix.{IMatrix, Matrix}
import org.goldenport.bag.{EmptyBag, ChunkBag, StringBag}
import org.goldenport.xml.dom.DomUtils
import org.goldenport.xml.xpath.XPathPredicate
import org.goldenport.log.Loggable
import org.goldenport.cli.ShellCommand
import org.goldenport.incident.{Incident => LibIncident}
import org.goldenport.values.PathName
import org.goldenport.parser.InterpolationParser
import org.goldenport.sexpr._, SExprConverter._
import org.goldenport.sexpr.eval.LispFunction._

/*
 * @since   Sep. 10, 2018
 *  version Sep. 29, 2018
 *  version Oct. 30, 2018
 *  version Jan.  3, 2019
 *  version Feb. 28, 2019
 *  version Mar. 30, 2019
 *  version Apr. 22, 2019
 *  version May. 26, 2019
 *  version Jun. 30, 2019
 *  version Jul. 28, 2019
 *  version Aug. 25, 2019
 *  version Sep. 30, 2019
 *  version Oct. 11, 2019
 *  version Nov. 30, 2019
 *  version Dec.  2, 2019
 *  version Jan. 30, 2020
 *  version Feb. 29, 2020
 *  version Mar.  1, 2020
 *  version May. 13, 2020
 *  version Jul. 20, 2020
 *  version Feb. 22, 2021
 *  version Mar. 21, 2021
 *  version Apr. 20, 2021
 *  version May. 20, 2021
 *  version Jun. 26, 2021
 *  version Nov. 29, 2021
 *  version Dec. 20, 2021
 *  version Feb.  9, 2022
 *  version Mar. 27, 2022
 *  version Apr. 16, 2022
 *  version May.  6, 2022
 * @version Aug. 31, 2022
 * @author  ASAMI, Tomoharu
 */
trait LispFunction extends PartialFunction[LispContext, LispContext]
    with UtilityPart with MatrixPart with XPathPart with RecordPart with TablePart with Loggable {
  def specification: FunctionSpecification
  def kindName: String
  def name: String = specification.name
  def isAcceptError: Boolean = false

  def isDefinedAt(p: LispContext): Boolean = {
    val r = is_defined_at(p)
    p.log.trace(s"isDefinedAt($name: ${p.value}): $r")
    r
  }

  protected def is_defined_at(p: LispContext): Boolean = p.value match {
    case m: SCell => p.args match {
      case Nil => false
      case x :: xs => x match {
        case a: SAtom if (a.name == name) => true
        case a: SAtom =>
          p.log.trace(s"$name, ${a.name}")
          false
        case _ => false
      }
    }
    case _ => false
  }

  def apply(p: LispContext): LispContext

  protected final def to_string(u: LispContext, p: SExpr): String = u.printString(p)

  protected final def format_string(u: LispContext, p: SExpr): String = u.formatString(p)

  protected final def string_interpolate(u: LispContext, ps: Seq[SExpr]): SExpr =
    SString(ps.map(string_interpolate_string(u, _)).mkString)

  protected final def string_interpolate_string(u: LispContext, p: SExpr): String = p match {
    case SString(s) => InterpolationParser.parse(s).map {
      case Right(r) => to_string(u, u.eval(SScript.create(r)))
      case Left(l) => l
    }.mkString
    case m => to_string(u, m)
  }

  protected final def string_interpolate_format(u: LispContext, ps: Seq[SExpr]): SExpr =
    SString(ps.map(string_interpolate_format(u, _)).mkString)

  protected final def string_interpolate_format(u: LispContext, p: SExpr): String = p match {
    case SString(s) => InterpolationParser.parse(s).map {
      case Right(r) => format_string(u, u.eval(SScript.create(r)))
      case Left(l) => l
    }.mkString
    case m => format_string(u, m)
  }

  protected final def string_concatenate(u: LispContext, ps: Seq[SExpr]): SExpr =
    SString(ps.map(to_string(u, _)).mkString)

  protected final def string_format(u: LispContext, template: String, ps: Seq[SExpr]): SExpr =
    SString(ps.map(x => u.formatString(template, x)).mkString)

  protected final def string_message(u: LispContext, template: String, ps: Seq[SExpr]): SExpr =
    SString(u.formatMessage(template, ps))

  protected final def string_message_key(u: LispContext, key: String, ps: Seq[SExpr]): SExpr =
    SString(u.formatMessageKey(key, ps))

  protected final def normalize_auto_urx(u: LispContext, p: SExpr): SExpr = p match {
    case m: SString =>
      val x = SUri.makeUrxOption(m.string)
      x.map(normalize_auto(u, _)).getOrElse(normalize_auto(u, p))
    case _ => normalize_auto(u, p)
  }

  protected final def normalize_auto(u: LispContext, p: SExpr): SExpr = p match {
    case m: SUrl => resolve_url(u, m)
    case m: SUri => resolve_uri(u, m)
    case m: SUrn => resolve_urn(u, m)
    case _ => SExpr.normalizeAuto(p)
  }

  protected final def resolve_url(u: LispContext, url: SUrl): SExpr = {
    val r = url_get(u, url)
    r.value
  }

  // protected final def resolve_url(u: LispContext, url: SUrl): SExpr = {
  //   val start = System.currentTimeMillis
  //   try {
  //     val res = u.serviceLogic.fileFetch(url.url)
  //     log_debug(s"resolve_url: $url => $res")
  //     val i = FileIncident(start, url, res)
  //     response_result(u, url, res)
  //   } catch {
  //     case NonFatal(e) =>
  //       log_debug(s"resolve_url: $url => $e")
  //       val i = FileIncident(start, url, e)
  //       SError(i)
  //   }
  // }

  protected final def resolve_uri(u: LispContext, uri: URI): SExpr = {
    // TODO URN and well known URI
    val url = SUrl(uri.toURL)
    resolve_url(u, url)
  }

  protected final def resolve_resource(u: LispContext, p: ResourceHandle): SExpr = {
    val url = SUrl(p.url)
    resolve_url(u, url)
  }

  protected final def file_fetch(u: LispContext, url: URL): LispContext = {
    val start = System.currentTimeMillis
    try {
      val res = u.serviceLogic.fileFetch(url)
      log_debug(s"file_fetch: $url => $res")
      val i = FileIncident(start, url, res)
      val r = response_result(u, url, res)
      u.toResult(r, i)
    } catch {
      case NonFatal(e) =>
        log_debug(s"file_fetch: $url => $e")
        val i = FileIncident(start, url, e)
        u.toResult(SError(i), i)
    }
  }

  protected final def resolve_urn(u: LispContext, urn: SUrn): SExpr =
    u.resolveUrn(urn)

  protected final def uri_save(
    u: LispContext,
    uri: URI,
    p: SExpr,
    charset: Option[Charset] = None
  ): SExpr = {
    val res = p.toBag match {
      case Right(c) =>
        val cs = charset getOrElse u.i18nContext.charset
        u.serviceLogic.fileSave(uri, c, cs)
        c
      case Left(b) =>
        u.serviceLogic.fileSave(uri, b)
        b
    }
    u.traceContext.effect(Effect.Io.Storage.File.create(uri))
    response_result(u, SUrl(uri.toURL), res)
  }

  protected final def url_get(u: LispContext, url: URL): LispContext = {
    url.getProtocol match {
      case "http" => http_get(u, url)
      case "https" => http_get(u, url)
      case "file" => file_fetch(u, url)
      case m => RAISE.invalidArgumentFault(s"Unavaiable protocol: $m")
    }
  }

  protected final def http_get(u: LispContext): LispContext =
    http_call(Request.GET, u)

  protected final def http_get(u: LispContext, url: URL): LispContext =
    http_call(Request.GET, u, url)

  protected final def http_post(u: LispContext): LispContext =
    http_call(Request.POST, u)

  protected final def http_put(u: LispContext): LispContext =
    http_call(Request.PUT, u)

  protected final def http_delete(u: LispContext): LispContext =
    http_call(Request.DELETE, u)

  protected final def http_call(method: Request.Method, u: LispContext): LispContext = {
    val req = build_request(method, u)
    u.toResult(http_call(u, req))
  }

  protected final def http_call(method: Request.Method, u: LispContext, url: URL): LispContext = {
    val start = System.currentTimeMillis
    def elements = u.evalElements
    def functionname = elements.functionName
    val req = Request(url, method, Record.empty, Record.empty, Record.empty)
    try {
      val res = u.serviceLogic.httpService(req)
      log_debug(s"http_call: $req => $res")
      val i = RestIncident(start, req, res)
      val r = response_result(req, res)
      if (res.isSuccess)
        u.toResult(r, i)
      // else if (res.isNotFound)
      //   u.toResult(SError.functionNotFound(functionname)).
      //     withUnavailableFunction(functionname)
      else
        u.toResult(SError(i), i)
      //u.toResult(http_call(u, req))
    } catch {
      case NonFatal(e) =>
        log_debug(s"http_call: $req => $e")
        val i = RestIncident(start, req, e)
        u.toResult(SError(i), i)
    }
  }

  protected final def http_call(u: LispContext, req: Request): SExpr = {
    val start = System.currentTimeMillis
    try {
      val res = u.serviceLogic.httpService(req)
      log_debug(s"http_call: $req => $res")
      if (res.isSuccess)
        u.toSExpr(res)
      else
        SError(start, req, res)
    } catch {
      case NonFatal(e) => SError(start, req, e)
    }
  }

  protected final def build_request(
    method: Request.Method,
    p: LispContext
  ): Request = build_request(
    method,
    p.parameters,
    p.bindings.getUrl(PROP_HTTP_BASEURL),
    p.bindings.getStringList(PROP_HTTP_HEADER)
  )

  protected final def build_request(
    method: Request.Method,
    parameters: Parameters,
    baseurl: Option[URL],
    header: Option[List[String]]
  ): Request = {
    val uri = parameters.argument1[URI](specification)
    // println(s"build_request: $uri")
    val query = parameters.getProperty('query).map(build_request_record)
    val form = parameters.getProperty('form).map(build_request_record)
    val url = try {
      uri.toURL
    } catch {
      case NonFatal(e) => baseurl.
          map(x => new URL(_normalize_baseurl(x), uri.toASCIIString)).
          getOrElse(RAISE.invalidArgumentFault(s"Unresolved uri: $uri. Set ${PROP_HTTP_BASEURL} to resolve relative URI."))
    }
    // println(s"build_request url: $baseurl")
    // println(s"build_request url: $url")
    val h = _build_header(header)
    Request(url, method, query.orZero, form.orZero, h)
  }

  private def _normalize_baseurl(p: URL): URL = UrlUtils.normalizeBaseUrl(p)

  protected final def build_request_record(p: SExpr): Record = p match {
    case m: SList => m.list./:(Record.empty)((z, x) => z + build_request_record_element(x))
    case m: SString => build_request_record_element(p)
    case m => RAISE.syntaxErrorFault(p.asString)
  }

  protected final def build_request_record_element(p: SExpr): Record = p match {
    case SNil => Record.empty
    case m: SCell => _build_cell(m)
    case m: SString => _build_string(m.string)
    case m => RAISE.syntaxErrorFault(p.asString)
  }

  private def _build_cell(p: SCell): Record = p.cdr match {
    case SNil => RAISE.notImplementedYetDefect
    case m: SCell => RAISE.notImplementedYetDefect
    case m => Record.data(p.car.asString -> p.cdr.asString)
  }

  private def _build_string(p: String): Record = {
    Request.parseQuery(p)
  }

  private def _build_header(p: Option[List[String]]): Record =
    p.map { x =>
      Record.createOption(
        x.map(s =>
          Strings.tokeyvalue(s) match {
            case ("", _) => "" -> None
            case (k, v) => k -> Some(v)
          }
        )
      )
    }.getOrElse(Record.empty)

  protected final def response_result(req: Request, p: Response): SExpr = {
    def string = p.getString orElse p.getBinary.map(_.toText) getOrElse ""
    def binary = getbinary getOrElse EmptyBag
    def getbinary: Option[ChunkBag] = p.getBinary orElse p.getString.map(StringBag.create)
    val mime = if (Strings.blankp(p.mime.name))
      MimeType.getBySuffix(req.url)
    else
      Some(p.mime)
    mime.map(m =>
      if (m.isHtml)
        SHtml(string)
      else if (m.isXml)
        SXml(string)
      else if (m.isJson)
        SJson(string)
      else if (m.isText)
        p.getString.map(SString).orElse(getbinary.map(SClob.apply)).getOrElse(SNil)
      else
        SBlob(binary)
    ).getOrElse(getbinary.map(SBlob.apply) getOrElse SNil)
  }

  protected final def response_result(
    u: LispContext,
    url: URL,
    p: ChunkBag
  ): SExpr = u.unmarshall(url, p)
}

trait ApplyFunction extends LispFunction {
  def kindName = "Apply"
}

trait EvalFunction extends LispFunction {
  def kindName = "Eval"
}

// trait ResolvedParametersFeature { self: EvalFunction =>
//   override def apply(p: LispContext): LispContext = {
//     val ps = p.parameters.map(normalize_auto(p, _))
//     val r = eval(ps)
//     p.toResult(r)
//   }
// }

trait ParameterEvalFunction extends EvalFunction {
  protected def is_normalize_auto: Boolean = true

  def apply(p: LispContext): LispContext = {
    val resolved = p.parameters // specification.resolve(p.parameters)
    val params = if (is_normalize_auto)
      resolved.map(normalize_auto(p, _))
    else
      resolved
    val r = eval(params)
    p.toResult(r)
  }

  def eval(p: Parameters): SExpr
}

trait CursorEvalFunction extends EvalFunction {
  protected def is_normalize_auto: Boolean = true

  def apply(u: LispContext): LispContext = {
    val resolved = u.parameters // specification.resolve(p.parameters)
    val params = if (is_normalize_auto)
      resolved.map(normalize_auto(u, _))
    else
      resolved
    val a = eval(u)
    val r = a.run(u.param.cursor(specification, params))
    u.toResult(r)
  }

  def eval(u: LispContext): CursorResult

  protected final def param_argument(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.argument(name)

  protected final def param_argument_option(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.argumentOption(name)

  protected final def param_argument_int_option(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.argumentOption(name, XInt)

  protected final def param_property_option(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.propertyOption(name)

  protected final def param_property_int_option(name: String): FunctionSpecification.Parameter =
    FunctionSpecification.Parameter.propertyOption(name, XInt)
}

trait ControlFunction extends LispFunction {
  def kindName = "Control"
}

trait HeavyFunction extends LispFunction { // CPU bound
  def kindName = "Heavy"
}

trait EffectFunction extends LispFunction {
  def kindName = "Effect"

  def applyEffect(p: LispContext): UnitOfWorkFM[LispContext]
}

trait IoFunction extends EffectFunction { // I/O bound
  override def kindName = "Io"

  protected final def execute_shell_command(
    p: LispContext
  ): SExpr = {
    val elements = p.evalElements
    val name = elements.functionName
    val commands = name :: elements.parameters.asStringList
    val env = Map.empty[String, String] // TODO
    val in = p.getPipelineIn
    execute_shell_command(p, commands, env, None, in, None)
  }

  protected final def execute_shell_command(
    u: LispContext,
    commands: String
  ): SExpr = execute_shell_command(u, Strings.totokens(commands, " "))

  protected final def execute_shell_command(
    u: LispContext,
    commands: Seq[String]
  ): SExpr = execute_shell_command(u, commands, Map.empty, None, None, None)

  protected final def execute_shell_command(
    u: LispContext,
    commands: Seq[String],
    env: Map[String, String],
    dir: Option[File],
    in: Option[SExpr],
    timeout: Option[Duration]
  ): SExpr = {
    val is = in.flatMap(_.getInputSource)
    val scmd = new ShellCommand(commands, env, dir, is, timeout)
    val result = scmd.execute
    SWait(name, { () =>
      val code = result.waitFor
      // println(s"execute_shell_command: $commands => $code")
      if (code == 0)
        _success(u, result)
      else
        _failure(u, scmd, result)
    })
  }

  private def _success(c: LispContext, p: ShellCommand.Result) = {
    val (props, stdout, _) = _properties(p)
    c.toResult(stdout, props)
  }

  private def _failure(c: LispContext, cmd: ShellCommand, p: ShellCommand.Result) = {
    val (props, stdout, stderr) = _properties(p)
    val aux = stderr.getString.flatMap(s =>
      Strings.tolines(s).find(_.nonEmpty).map(x => s": $x")
    ).getOrElse("")
    val msg = s"Shell Command[${cmd.show}](${p.waitFor})${aux}"
    val error = SError(msg, stdout, stderr)
    c.toResult(error, props)
  }

  private def _properties(p: ShellCommand.Result): (Record, SBlob, SBlob) = {
    val retval = SNumber(p.waitFor)
    val stdout = SBlob(p.stdout)
    val stderr = SBlob(p.stderr)
    val a = Record.data(
      "return-code" -> retval,
      "stdout" -> stdout,
      "stderr" -> stderr
    )
    (a, stdout, stderr)
  }

  protected final def is_implicit_http_communication(u: LispContext): Boolean =
    u.bindings.getUrl("http.baseurl").isDefined
}

trait AsyncIoFunction extends IoFunction { // I/O bound, implicit asynchronous in function
  override def kindName = "AsyncIo"
}

trait SyncIoFunction extends IoFunction { // I/O bound, synchronous is required.
  override def kindName = "SyncIo"
}

object LispFunction {
  type ValidationResult = ValidationNel[SError, SExpr]
  type CursorResult = State[Parameters.Cursor, ValidationResult]

  val PROP_HTTP_BASEURL = "http.baseurl"
  val PROP_HTTP_HEADER = "http.header"

  case object EvalOrInvoke extends ControlFunction {
    val specification = FunctionSpecification("eval-or-invoke", 1)
    def apply(p: LispContext) = {
      p.parameters.arguments.head match {
        case m: SAtom =>
          // println(s"eval-or-invoke: ${p}")
          def newctx = p.toResult(SList(m))
          p.getBindedValue(m.name).map {
            case mm: SLambda => p.apply(SList(mm))
            case mm => p.toResult(mm)
          }.getOrElse(p.createDynamicServiceFunction(name).apply(newctx))
        case m => RAISE.notImplementedYetDefect
      }
    }
  }

  case object ReturnSuccess extends ControlFunction {
    val specification = FunctionSpecification("return-success", 3)
    def apply(p: LispContext) = {
      val params = p.parameters
      def error = params.getProperty('else) getOrElse SError("return-success failure")
      val r = params.arguments match {
        case out :: predicate :: in :: Nil =>
          if (p.evalCondition(predicate, in))
            p.eval(out)
          else
            p.eval(error)
        case _ => SError("return-success too many arguments")
      }
      p.toResult(r)
    }
  }

  case object Atom extends ParameterEvalFunction {
    val specification = FunctionSpecification("atom", 1)
    def eval(p: Parameters) = {
      val x = p.argument1[SExpr](specification)
      SBoolean(x.isInstanceOf[SAtom])
    }
  }

  case object Eq extends ParameterEvalFunction {
    val specification = FunctionSpecification("eq", 2)
    def eval(p: Parameters) = {
      val (lhs, rhs) = p.argument2[SExpr, SExpr](specification)
      SBoolean(lhs eq rhs)
    }
  }

  case object Cons extends ParameterEvalFunction {
    val specification = FunctionSpecification("cons", 2)
    def eval(p: Parameters) = {
      val (lhs, rhs) = p.argument2[SExpr, SExpr](specification)
      SCell(lhs, rhs)
    }
  }

  case object Quote extends ControlFunction {
    val specification = FunctionSpecification("quote", 1)
    def apply(p: LispContext) = p.toResult(p.parameters.arguments.head)
  }

  case object Setq extends ControlFunction {
    val specification = FunctionSpecification("setq", 2)
    def apply(p: LispContext) = {
      val (atom, v) = p.parameters.argument2[SAtom, SExpr](specification)
      val r = p.eval(v)
      p.toResult(r, Record.data(atom.name -> r))
    }
  }

  case object Car extends ParameterEvalFunction {
    val specification = FunctionSpecification("car", 1)
    def eval(p: Parameters) =
      p.arguments.headOption.map {
        case m: SCell => m.car
        case m => SError(s"Not list: $m")
      }.getOrElse(SError("Empty list"))
  }

  case object Cdr extends ParameterEvalFunction {
    val specification = FunctionSpecification("cdr", 1)
    def eval(p: Parameters) =
      p.arguments.headOption.map {
        case m: SCell => m.cdr
        case m => SError(s"Not list: $m")
      }.getOrElse(SError("Empty list"))
  }

  case object Cond extends ControlFunction {
    val specification = FunctionSpecification("cond", 1)
    def apply(p: LispContext) = {
      val r = p.parameters.arguments.toStream.flatMap(_cond(p)).headOption.getOrElse(SError("cond: no selection"))
      p.toResult(r)
    }

    private def _cond(ctx: LispContext)(p: SExpr): Option[SExpr] = p match {
      case SCell(car, cdr) =>
        if (ctx.evalCondition(car))
          Some(ctx.eval(_value(cdr)))
        else
          None
      case _ => None
    }

    private def _value(p: SExpr) = p match {
      case SCell(car, cdr) => car
      case _ => SError("cond: Illegal structure")
    }
  }

  case object If extends ControlFunction {
    val specification = FunctionSpecification("if", 2)
    def apply(p: LispContext) = {
      val (pred, thenpart) = p.parameters.argument2[SExpr, SExpr](specification)
      def elsepart = p.parameters.arguments.lift(2).getOrElse(SNil)
      val r = if (p.evalCondition(pred))
        thenpart
      else
        elsepart
      p.toResult(p.eval(r))
    }
  }

  case object ListFunc extends ParameterEvalFunction {
    val specification = FunctionSpecification("list")
    def eval(p: Parameters) = SList.create(p.arguments)
  }

  case object And extends ParameterEvalFunction {
    val specification = FunctionSpecification("and")
    def eval(p: Parameters) = {
      @annotation.tailrec
      def go(x: List[SExpr]): SExpr = x match {
        case Nil => SBoolean.TRUE
        case x :: Nil => if (x.isNilOrFalse) SNil else x
        case x :: xs => if (x.isNilOrFalse) SNil else go(xs)
      }
      go(p.arguments)
    }
  }

  case object Or extends ParameterEvalFunction {
    val specification = FunctionSpecification("or")
    def eval(p: Parameters) = {
      @annotation.tailrec
      def go(x: List[SExpr]): SExpr = x match {
        case Nil => SNil
        case x :: Nil => if (x.isNilOrFalse) SNil else x
        case x :: xs => if (x.isNilOrFalse) go(xs) else x
      }
      go(p.arguments)
    }
  }

  case object Equal extends ParameterEvalFunction {
    val specification = FunctionSpecification("=", 2)

    def eval(p: Parameters) = _equal(p.arguments(0), p.arguments(1))

    private def _equal(l: SExpr, r: SExpr): SExpr = SBoolean(l == r)
  }

  // invariant
  case object Assert extends CursorEvalFunction {
    val specification = FunctionSpecification("assert",
      param_argument("predicate"),
      param_argument("target")
    )

    def eval(u: LispContext): CursorResult = for {
      pred <- u.param.take('predicate)
      target <- u.param.take('target)
    } yield (pred |@| target)(_assert(u))

    private def _assert(u: LispContext)(pred: SExpr, target: SExpr): SExpr =
      if (u.evalCondition(pred, target))
        target
      else
        SError.invariant(target)
  }

  // pre-condition (caller responsibility)
  case object Assume extends CursorEvalFunction {
    val specification = FunctionSpecification("assume",
      param_argument("predicate"),
      param_argument("target")
    )

    def eval(u: LispContext): CursorResult = for {
      pred <- u.param.take('predicate)
      target <- u.param.take('target)
    } yield (pred |@| target)(_assume(u))

    private def _assume(u: LispContext)(pred: SExpr, target: SExpr): SExpr =
      if (u.evalCondition(pred, target))
        target
      else
        SError.preConditionState(target)
  }

  // pre-condition (callee responsibility)
  case object Require extends CursorEvalFunction {
    val specification = FunctionSpecification("require",
      param_argument("predicate"),
      param_argument("target")
    )

    def eval(u: LispContext): CursorResult = for {
      pred <- u.param.take('predicate)
      target <- u.param.take('target)
    } yield (pred |@| target)(_require(u))

    private def _require(u: LispContext)(pred: SExpr, target: SExpr): SExpr =
      if (u.evalCondition(pred, target))
        target
      else
        SError.preCondition(target)
  }

  // post-condition
  case object Ensuring extends CursorEvalFunction {
    val specification = FunctionSpecification("ensuring",
      param_argument("predicate"),
      param_argument("target")
    )

    def eval(u: LispContext): CursorResult = for {
      pred <- u.param.take('predicate)
      target <- u.param.take('target)
    } yield (pred |@| target)(_ensuring(u))

    private def _ensuring(u: LispContext)(pred: SExpr, target: SExpr): SExpr =
      if (u.evalCondition(pred, target))
        target
      else
        SError.postCondition(target)
  }

  case object Plus extends ParameterEvalFunction {
    val specification = FunctionSpecification.variableArityArgument("+", 2, XDecimal)

    def eval(p: Parameters) = _go(p.arguments.head, p.arguments.tail)
    //   SNumber(p.asBigDecimalList.sum)
    // }

    @annotation.tailrec
    private def _go(p: SExpr, ps: List[SExpr]): SExpr = ps match {
      case Nil => p
      case x :: xs => _go(_plus(p, x), xs)
    }

    private def _plus(l: SExpr, r: SExpr): SExpr = (l, r) match {
      case (ml: SNumber, mr: SNumber) => ml + mr
      case (ml: SVector, mr: SVector) => ml + mr
      case (ml: SMatrix, mr: SMatrix) => ml + mr
      case _ => SError(s"Invalid number or vector or matrix: $l, $r")
    }
  }

  case object Minus extends ParameterEvalFunction {
    val specification = FunctionSpecification("-", 2)

    def eval(p: Parameters) = _go(p.arguments.head, p.arguments.tail)

    @annotation.tailrec
    private def _go(p: SExpr, ps: List[SExpr]): SExpr = ps match {
      case Nil => p
      case x :: xs => _go(minus(p, x), xs)
    }

    private def minus(l: SExpr, r: SExpr): SExpr = (l, r) match {
      case (ml: SNumber, mr: SNumber) => ml - mr
      case (ml: SVector, mr: SVector) => ml - mr
      case (ml: SMatrix, mr: SMatrix) => ml - mr
      case _ => SError(s"Invalid number or vector or matrix: $l, $r")
    }
  }

  case object Multify extends ParameterEvalFunction {
    val specification = FunctionSpecification("*", 2)

    def eval(p: Parameters) = _go(p.arguments.head, p.arguments.tail)

    @annotation.tailrec
    private def _go(p: SExpr, ps: List[SExpr]): SExpr = ps match {
      case Nil => p
      case x :: xs => _go(_multify(p, x), xs)
    }

    private def _multify(l: SExpr, r: SExpr): SExpr = (l, r) match {
      case (ml: SNumber, mr: SNumber) => ml * mr
      case (ml: SNumber, mr: SMatrix) => ml * mr
      case (ml: SMatrix, mr: SNumber) => ml * mr
      case (ml: SMatrix, mr: SVector) => ml * mr
      case (ml: SMatrix, mr: SMatrix) => ml * mr
      case _ => SError(s"Invalid number or vecror or matrix: $l, $r")
    }
  }

  case object Divide extends ParameterEvalFunction {
    val specification = FunctionSpecification("/", 2)

    def eval(p: Parameters) = _go(p.arguments.head, p.arguments.tail)

    @annotation.tailrec
    private def _go(p: SExpr, ps: List[SExpr]): SExpr = ps match {
      case Nil => p
      case x :: xs => _go(_divide(p, x), xs)
    }

    private def _divide(l: SExpr, r: SExpr): SExpr = (l, r) match {
      case (ml: SNumber, mr: SNumber) => ml / mr
//      case (ml: SNumber, mr: SMatrix) => ml / mr
//      case (ml: SMatrix, mr: SNumber) => ml / mr
//      case (ml: SMatrix, mr: SMatrix) => ml / mr
      case _ => SError(s"Invalid number or vector or matrix: $l, $r")
    }
  }

  case object Length extends ParameterEvalFunction {
    val specification = FunctionSpecification("length", 1)
    def eval(p: Parameters) = {
      val r = p.arguments.map {
        case m: SString => m.string.length
        case m: SList => m.length
        case m => m.asString.length
      }.sum
      SNumber(r)
    }
  }

  case object Inv extends ParameterEvalFunction {
    val specification = FunctionSpecification("inv", 1)
    def eval(p: Parameters) = {
      val a = p.argument1[IMatrix[Double]](specification)
      val r = a.inv
      SMatrix(r)
    }
  }

  case object StringInterpolate extends ApplyFunction {
    val specification = FunctionSpecification("string-interpolate", 1)
    def apply(u: LispContext): LispContext = {
      val a = u.parameters.arguments
      val r = string_interpolate(u, a)
      u.toResult(r)
    }
  }

  case object StringInterpolateFormat extends ApplyFunction {
    val specification = FunctionSpecification("string-interpolate-format", 1)
    def apply(u: LispContext): LispContext = {
      val a = u.parameters.arguments
      val r = string_interpolate_format(u, a)
      u.toResult(r)
    }
  }

  case object StringFormat extends ApplyFunction {
    val specification = FunctionSpecification("string-format", 1)
    def apply(u: LispContext): LispContext = {
      val a = for {
        template <- u.param.argument1[String]
        args <- u.param.arguments
      } yield {
        (template |@| args) { (t, xs) =>
          string_format(u, t, xs)
        }
      }
      val r = a.run(u.param.cursor(specification))
      u.toResult(r)
    }
  }

  case object StringMessage extends ApplyFunction {
    val specification = FunctionSpecification("string-message", 1)
    def apply(u: LispContext): LispContext = {
      val a = for {
        template <- u.param.argument1[String]
        args <- u.param.arguments
      } yield {
        (template |@| args) { (t, xs) =>
          string_message(u, t, xs)
        }
      }
      val r = a.run(u.param.cursor(specification))
      u.toResult(r)
    }
  }

  case object StringMessageKey extends ApplyFunction {
    val specification = FunctionSpecification("string-message-key", 1)
    def apply(u: LispContext): LispContext = {
      val a = for {
        key <- u.param.argument1[String]
        args <- u.param.arguments
      } yield {
        (key |@| args) { (k, xs) =>
          string_message_key(u, k, xs)
        }
      }
      val r = a.run(u.param.cursor(specification))
      u.toResult(r)
    }
  }

  case object Pop extends ApplyFunction {
    val specification = FunctionSpecification("pop")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      params.getArgument1[Int](specification).map(x => p.pop(x)).getOrElse(p.pop)
    }
  }

  case object Peek extends ApplyFunction {
    val specification = FunctionSpecification("peek")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      val r = params.getArgument1[Int](specification).map(x => p.peek(x)).getOrElse(p.peek)
      p.toResult(r)
    }
  }

  case object Mute extends ParameterEvalFunction {
    val specification = FunctionSpecification("mute")
    def eval(p: Parameters) = SMute(p.head)
  }

  case object History extends ApplyFunction {
    val specification = FunctionSpecification("history")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      val r = params.getArgument1[Int](specification).map(x => p.takeHistory(x)).getOrElse(p.takeHistory)
      p.toResult(r)
    }
  }

  // TODO
  case object CommandHistory extends ApplyFunction { // XXX call, invoke, request, command ?
    val specification = FunctionSpecification("command-history")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      val r = params.getArgument1[Int](specification).map(x => p.takeCommandHistory(x)).getOrElse(p.takeCommandHistory)
      p.toResult(r)
    }
  }

  case object PathGet extends CursorEvalFunction {
    import javax.xml.namespace.QName
    import javax.xml.xpath._
    import org.apache.commons.jxpath._
    import org.goldenport.record.v2._
    import org.goldenport.record.v3.jxpath.RecordJxPathContext

    val specification = FunctionSpecification("path-get", 2)

    case class ReturnType(
      datatype: DataType,
      xpath: QName,
      auto: Boolean = false
    )
    object ReturnType {
      val autoType = ReturnType(XXml, XPathConstants.NODESET, true)
      val booleanType = ReturnType(XBoolean, XPathConstants.BOOLEAN)
      val numberType = ReturnType(XDecimal, XPathConstants.NUMBER)
      val stringType = ReturnType(XString, XPathConstants.STRING)
      val nodeType = ReturnType(XXml, XPathConstants.NODE)
      val nodesetType = ReturnType(XXml, XPathConstants.NODESET)

      def apply(p: Option[String]): ReturnType = p.map(apply).getOrElse(autoType)

      def apply(p: String): ReturnType = p match {
        case "auto" => autoType
        case "boolean" => booleanType
        case "number" => numberType
        case "string" => stringType
        case "node" => nodeType
        case "nodeset" => nodesetType
        case m => DataType.get(m).map(apply).getOrElse(
          RAISE.invalidArgumentFault(m)
        )
      }

      def apply(p: DataType): ReturnType = p match {
        case XBoolean => booleanType
        case XDecimal => numberType
        case XString => stringType
        case m => ReturnType(m, XPathConstants.STRING)
      }

      def parse(p: Option[String]): Consequence[ReturnType] = Consequence(apply(p))
    }

    // override def isDefinedAt(p: LispContext): Boolean =
    //   // p.value.isInstanceOf[SXPath] || is_defined_at(p)
    //   is_defined_at(p)

    // def eval(p: Parameters) = {
    //   val returntype: ReturnType = ReturnType(p.getPropertyString('type))
    //   val (xpath, x) = p.arguments(0) match {
    //     case m: SXPath => (m, p.arguments(1))
    //     case m => p.arguments(1) match {
    //       case mm: SXPath => (mm, m)
    //       case mm => RAISE.syntaxErrorFault(s"No xpath both ${m} and ${mm}")
    //     }
    //   }
    //   val target = x
    //   traverse(returntype, xpath, target)
    // }

    def eval(u: LispContext): CursorResult = for {
      t <- u.param.getString('type)
      p1 <- u.param.argument
      p2 <- u.param.argument
    } yield (t |@| p1 |@| p2)(_traverse(u)(_, _, _))

    private def _traverse(u: LispContext)(t: Option[String], p1: SExpr, p2: SExpr): SExpr =
      SExpr.run {
        for {
          rt <- ReturnType.parse(t)
        } yield p1 match {
          case m: SXPath => traverse(u, rt, m, p2)
          case _ => p2 match {
            case mm: SXPath => traverse(u, rt, mm, p1)
            case _ => SError(s"No xpath both ${p1} and ${p2}")
          }
        }
      }

    def traverse(u: LispContext, rtype: ReturnType, xpath: SXPath, target: SExpr): SExpr = {
      val start = u.dateTimeContext.timestamp
      _traverse(rtype, xpath, target) match {
        case Some(s) =>
          s
        case None => 
          val i = XPathIncident.notFound(start, u.dateTimeContext.timestamp, xpath, target)
          SError(i)
      }
    }

    def traverse(rtype: ReturnType, xpath: SXPath, target: SExpr): SExpr =
      _traverse(rtype, xpath, target) getOrElse SError.notFoundIn(xpath, target)

    private def _traverse(rtype: ReturnType, xpath: SXPath, target: SExpr): Option[SExpr] = try {
      target match {
        case m: SXml => _traverse_xml(rtype, xpath, m)
        case m: SHtml => _traverse_html(rtype, xpath, m)
        case m: SJson => _traverse_json(rtype, xpath, m)
        case m: SRecord => _traverse_record(rtype, xpath, m)
        case m: STable => _traverse_table(rtype, xpath, m)
        case m => _traverse_bean(rtype, xpath, m) // SError.syntaxError(s"Unaviable for xpath: $m")
      }
    } catch {
      case NonFatal(e) => Some(SError(e))
    }

    private def _traverse_xml(rtype: ReturnType, xpath: SXPath, p: SXml) =
      _traverse_dom_xpath(rtype, xpath, p.dom)

    private def _traverse_dom_xpath(
      rtype: ReturnType,
      xpath: SXPath,
      p: org.w3c.dom.Node
    ): Option[SExpr] = {
      val engine = XPathFactory.newInstance().newXPath()
      // val s = DomUtils.toPrettyText(p)
      // val d = DomUtils.parseXml(s)
      // println(DomUtils.toPrettyText(p))
      val r = Option(engine.compile(xpath.path).evaluate(p, rtype.xpath))
      val x0 = rtype.xpath match {
        case XPathConstants.BOOLEAN => r
        case XPathConstants.NUMBER => r
        case XPathConstants.STRING => r.map(rtype.datatype.toInstance(_))
        case XPathConstants.NODE => r
        case XPathConstants.NODESET => r
      }
      val x = x0.map(a =>
        if (rtype.auto) {
          a match {
            case m: org.w3c.dom.Element =>
              if (DomUtils.children(m).forall(_.isInstanceOf[org.w3c.dom.Text]))
                m.getTextContent
              else
                m
            case m: org.w3c.dom.NodeList =>
              val xs = for (i <- 0 until m.getLength) yield m.item(i)
              if (xs.isEmpty)
                xs
              else if (DomUtils.isTextOnly(xs))
                xs.map(_.getTextContent).mkString
              else if (xs.forall(DomUtils.isTextOnlyChildren))
                if (xs.length == 1) {
                  xs.map(_.getTextContent).mkString
                } else {
                  xs.map(_.getTextContent)
                }
              else if (xs.length == 1)
                xs(0)
              else
                m
            case _ => a
          }
        } else {
          a
        }
      )
      x.map(SExpr.create)
    }

    // private def _traverse_xml_xpath_string(xpath: SXPath, p: SXml) =
    //   _traverse_dom_xpath_string(xpath, p.dom)

    // private def _traverse_xml_xpath_nodeset(xpath: SXPath, p: SXml) =
    //   _traverse_dom_xpath_nodeset(xpath, p.dom)

    // private def _traverse_dom_xpath_string(xpath: SXPath, p: org.w3c.dom.Node) = {
    //   val engine = XPathFactory.newInstance().newXPath()
    //   val r = engine.compile(xpath.path).evaluate(p, XPathConstants.STRING)
    //   println(s"_traverse_dom_xpath_string: $r")
    //   SExpr.create(r)
    // }

    // private def _traverse_dom_xpath_nodeset(xpath: SXPath, p: org.w3c.dom.Node) = {
    //   import javax.xml.xpath._
    //   val engine = XPathFactory.newInstance().newXPath()
    //   val r = engine.compile(xpath.path).evaluate(p, XPathConstants.NODESET)
    //   println(s"_traverse_dom_xpath_nodeset: $r")
    //   SExpr.create(r)
    // }

    // private def _traverse_html(rtype: ReturnType, xpath: SXPath, p: SHtml) =
    //   if (true)
    //     _traverse_dom_xpath_nodeset(xpath, p.dom)
    //   else
    //     _traverse_dom_xpath_string(xpath, p.dom)

    private def _traverse_html(rtype: ReturnType, xpath: SXPath, p: SHtml) =
      _traverse_dom_xpath(rtype, xpath, p.domForXpath)

    private def _traverse_json(rtype: ReturnType, xpath: SXPath, p: SJson): Option[SExpr] = {
      val pc = JXPathContext.newContext(p.json)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse_record(rtype: ReturnType, xpath: SXPath, p: SRecord): Option[SExpr] = {
      // val pc = JXPathContext.newContext(p.asObject)
      val pc = RecordJxPathContext.newContext(p.record)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse_table(rtype: ReturnType, xpath: SXPath, p: STable): Option[SExpr] = {
      val pc = JXPathContext.newContext(p.table)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse_bean(rtype: ReturnType, xpath: SXPath, p: SExpr): Option[SExpr] = {
      val pc = JXPathContext.newContext(p.asObject)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse(rtype: ReturnType, xpath: SXPath, pc: JXPathContext): Option[SExpr] = {
      def v0 = rtype.xpath match {
        case XPathConstants.BOOLEAN => pc.getValue(xpath.path, classOf[Boolean])
        case XPathConstants.NUMBER => pc.getValue(xpath.path, classOf[BigDecimal])
        case XPathConstants.STRING => rtype.datatype.toInstance(pc.getValue(xpath.path, classOf[String]))
        case XPathConstants.NODE => pc.getValue(xpath.path)
        case XPathConstants.NODESET => pc.getValue(xpath.path)
      }

      val v = if (rtype.auto) {
        pc.iterate(xpath.path).asScala.toList match {
          case Nil => None
          case x :: Nil => Some(x)
          case xs => Some(xs)
        }
      } else {
        Some(v0)
      }
      v.map(SExpr.create)
    }

    // private def _traverse_jxpath_value(xpath: String, pc: JXPathContext): SExpr = {
    //   // println(s"xpath: $xpath")
    //   // println(s"JXPathContext: ${pc.getContextBean}")
    //   import scala.collection.JavaConverters._
    //   val v = pc.getValue(xpath)
    //   SExpr.create(v)
    // }

    // private def _traverse_jxpath_iterator(xpath: String, pc: JXPathContext): SExpr = {
    //   import scala.collection.JavaConverters._
    //   val xs = pc.iteratePointers(xpath)
    //   xs.asScala.map { x =>
    //     val ptr = x.asInstanceOf[Pointer]
    //     SExpr.create(ptr.getNode)
    //   }.toList match {
    //     case Nil => SNil
    //     case x :: Nil => x
    //     case xs => SList.create(xs)
    //   }
    // }

    // private def _to_sexpr(p: Any): SExpr = p match {
    //   case m: String => SString(m)
    //   case m => RAISE.notImplementedYetDefect(m.toString)
    // }
  }

  case object Transform extends ParameterEvalFunction {
    val specification = FunctionSpecification("transform")

    def eval(p: Parameters) = {
      val (xslt, xml) = p.arguments(0) match {
        case m: SXsl => p.arguments(1) match {
          case mm: SXml => (m, mm)
          case mm => RAISE.syntaxErrorFault(s"No xml: ${mm}")
        }
        case m => RAISE.syntaxErrorFault(s"No xslt: ${m}")
      }
      _transform(xslt, xml)
    }

    private def _transform(xslt: SXsl, xml: SXml): SXml = {
      val r = DomUtils.transform(xslt.dom, xml.dom)
      SXml(r)
    }
  }

  case object Xslt extends ApplyFunction {
    val specification = FunctionSpecification("xslt")

    def apply(p: LispContext): LispContext = {
      val r = eval(p, p.parameters)
      p.toResult(r)
    }

    def eval(u: LispContext, p: Parameters) = {
      val a0 = _resolve(u, p.arguments(0))
      val a1 = _resolve(u, p.arguments(1))
      (a0, a1) match {
        case (l: SXsl, r: IDom) => _xslt(l, r)
        case (l: IDom, r: SXsl) => _xslt(r, l)
        case (l, r) => RAISE.syntaxErrorFault(s"Invalid xslt and xml: ${l}, ${r}")
      }
    }

    private def _resolve(u: LispContext, p: SExpr): SExpr = p match {
      case m: IDom => m
      case m: SUrl => resolve_url(u, m) match {
        case mm: IDom => mm
        case mm => RAISE.syntaxErrorFault(s"No xml in : ${m}")
      }
      case m => RAISE.syntaxErrorFault(s"No xml: ${m}")
    }

    private def _xslt(xslt: SXsl, p: IDom): SXml = {
      // println("XSLT: " + DomUtils.toString(p.dom))
      val r = DomUtils.transform(xslt.dom, p.dom)
      SXml(r)
    }
  }

  /*
   * select [projection] :from [table] :join [join] :where [query] :orderby [orderby] :limit [limit]
   * select [projection] [table]
   */
  case object Select extends CursorEvalFunction {
    val specification = FunctionSpecification("select",
      param_argument("projection"),
      param_argument("from"),
      param_property_option("join"),
      param_property_option("where"),
      param_property_option("orderby"),
      param_property_int_option("start"),
      param_property_int_option("limit")
    )

    // protected final def param_arguments = State[Parameters.Cursor, ValidationNel[SError, List[SExpr]]](_.arguments)

    def eval(u: LispContext): CursorResult =
      for {
        p <- u.param.take('projection)
        f <- u.param.take('from)
        j <- u.param.get('join)
        w <- u.param.get('where)
        o <- u.param.get('orderby)
        s <- u.param.getInt('start)
        l <- u.param.getInt('limit)
      } yield {
        // (p |@| f |@| j |@| w |@| o |@| s |@| l) { (projection, from, join, where, orderby, start, limit) =>
        //   _select(projection, from, join, where, orderby, start,limit)
        // }
        (p |@| f |@| j |@| w |@| o |@| s |@| l)(_select)
      }

    private def _select(
      projection: SExpr,
      from: SExpr,
      join: Option[SExpr],
      where: Option[SExpr],
      orderby: Option[SExpr],
      start: Option[Int],
      limit: Option[Int]
    ): SExpr = {
      projection match {
        case m: SList => _select(m, from)
        case m => RAISE.notImplementedYetDefect
      }
    }

    private def _select(form: SList, p: SExpr) = p match {
      case m: STable => _select_table(form, m)
      case m: SRecord => _select_record(form, m)
      case _ => _select_list(form, p)
    }

    private def _select_table(form: SList, p: STable): SExpr = {
      val names = form.list.traverse {
        case m: SXPath => _select_name(m)
        case m: SString => _select_name(m)
        case m: SAtom => _select_name(m)
        case m => ??? // ConclusionResult.ParseFailure("???") // SError.invalidArgumentFault(s"Invalid form element: $m")
      }
      SExpr.run {
        for (xs <- names) yield STable(p.table.select(xs))
      }
      // val names = for (x <- form.list) yield x match {
      //   case m: SXPath => _select_path(m, p)
      //   case m: SString => _select_string(m, p)
      //   case m: SAtom => _select_atom(m, p)
      //   case m => SError.invalidArgumentFault(s"Invalid form element: $m")
      // }
      // STable(p.table.select(names))
    }

    private def _select_record(form: SList, p: SRecord): SExpr = {
      val names = form.list.traverse {
        case m: SXPath => _select_name(m)
        case m: SString => _select_name(m)
        case m: SAtom => _select_name(m)
        case m => ??? // ConclusionResult.ParseFailure("???") // SError.invalidArgumentFault(s"Invalid form element: $m")
      }
      SExpr.run {
        for (xs <- names) yield SRecord(p.record.toRecord.select(xs))
      }
    }

    private def _select_list(form: SList, p: SExpr) = {
      val rs = for (x <- form.list) yield x match {
        case m: SXPath => _select_path(m, p)
        case m: SString => _select_string(m, p)
        case m: SAtom => _select_atom(m, p)
        case m => SError.invalidArgumentFault(s"Invalid form element: $m")
      }
      SList.create(rs)
    }

    private def _select_path(path: SXPath, p: SExpr) = {
      // val rtype = ReturnType()
      xpath_traverse_auto(path.path, p)
    }

    private def _select_string(path: SString, p: SExpr) = {
      SError.notImplementedYetDefect(s"$path")
    }

    private def _select_atom(path: SAtom, p: SExpr) = {
      SError.notImplementedYetDefect(s"$path")
    }

    private def _select_name(path: SXPath): Consequence[String] = {
      val a = PathName(path)
      a.components match {
        case Nil => Consequence.invalidArgumentFault("Empty name")
        case x :: Nil => Consequence.success(x)
        case xs => Consequence.invalidArgumentFault(I18NMessage("Invalid name: {0}", path))
      }
    }

    private def _select_name(s: SString): Consequence[String] =
      Consequence.success(s.string)

    private def _select_name(atom: SAtom): Consequence[String] =
      Consequence.success(atom.name)
  }

  case object Retry extends ControlFunction {
    val specification = FunctionSpecification("retry", 1)

    def apply(p: LispContext) = {
      val a = p.parameters.argument1[SExpr](specification)
      val expr = p.eval(a)
      val maxretry = 10
      val duration = 1.second
      val backoff = 2.0
      val retry = LibRetry.create(p.eval(expr), maxretry, duration, backoff, _is_io_error)
      val r = retry.run
      val incident: LibIncident = retry.incident
      p.toResult(r, incident)
    }

    private def _is_io_error(p: SExpr): LibRetry.Strategy = p match {
      case m: SError => m.exception.map(LibRetry.defaultRetryException).getOrElse(LibRetry.ErrorStrategy(s"$m"))
      case _ => LibRetry.SuccessStrategy
    }
  }

  case object Sh extends IoFunction { // TODO AsyncIoFunction
    val specification = FunctionSpecification("sh", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        strategy <- p.param.powertypeOption('resulttype, SExpr.CreateStrategy)
        
        args <- p.param.argumentList[String]
      } yield {
        (strategy |@| args) { (s, xs) =>
          val r = execute_shell_command(p, xs).resolve // IoFunction is implicitly on SFuture.
          val ri1 = SExpr.create(s, r)
          val ri2 = FunctionIncident(0L, "sh", ri1)
          (ri1, ri2)
        }
      }
      val ri = a.run(p.param.cursor(specification))
      p.toResultSI(ri)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object Fetch extends IoFunction {
    val specification = FunctionSpecification("fetch", 1)

    // override def isDefinedAt(p: LispContext): Boolean =
    //   is_defined_at(p)

    def apply(p: LispContext): LispContext = {
      val url = p.valueOrArgument1[URL](specification)
      url_get(p, url)
      // http_get(p, url)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  // TODO Consider the relationship with Fetch
  case object Load extends IoFunction {
    val specification = FunctionSpecification("load", 1)

    def apply(p: LispContext): LispContext = {
      val url = p.valueOrArgument1[URL](specification) // TODO uri
      url_get(p, url)
      // TODO eval
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object Save extends IoFunction {
    val specification = FunctionSpecification("save", 2)

    def apply(p: LispContext): LispContext = {
      val (uri, a) = p.parameters.uriSExpr
      val r = uri_save(p, uri, a)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object Print extends ParameterEvalFunction {
    val specification = FunctionSpecification("print", 1)
    def eval(p: Parameters) = {
      val r: String = p.arguments.map(_.print).concatenate
      print(r) // TODO console print
      SString(r)
    }
  }

  case object Println extends ParameterEvalFunction {
    val specification = FunctionSpecification("println", 1)
    def eval(p: Parameters) = {
      val r = p.arguments.map(_.print).concatenate
      println(r) // TODO console println
      SString(r)
    }
  }

  case object HttpGet extends IoFunction {
    val specification = FunctionSpecification("http-get", 2)

    def apply(p: LispContext): LispContext = http_get(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpPost extends IoFunction {
    val specification = FunctionSpecification("http-post", 2)

    def apply(p: LispContext): LispContext = http_post(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpPut extends IoFunction {
    val specification = FunctionSpecification("http-put", 2)

    def apply(p: LispContext): LispContext = http_put(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpDelete extends IoFunction {
    val specification = FunctionSpecification("http-delete", 2)

    def apply(p: LispContext): LispContext = http_delete(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object VectorVerticalFill extends ParameterEvalFunction {
    val specification = FunctionSpecification("vector-vertical-fill", 2)

    def eval(p: Parameters) = {
      val (count, v) = p.argument2[Int, Double](specification)
      SMatrix(Matrix.vectorVerticalFill(count, v))
    }
  }

  case object MatrixHorizontalConcatenate extends ParameterEvalFunction {
    val specification = FunctionSpecification("matrix-horizontal-concatenate", 2)

    def eval(p: Parameters) = {
      val xs = p.argumentNonEmptyVector[IMatrix[Double]](specification)
      SMatrix(Matrix.horizontalConcatenate(xs))
    }
  }

  case object MatrixLoad extends IoFunction {
    val specification = FunctionSpecification("matrix-load", 1)

    def apply(p: LispContext): LispContext = {
      val a = p.parameters.argument1[URI](specification)
      val r = matrix_load(p, a)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object MatrixSave extends IoFunction {
    val specification = FunctionSpecification("matrix-save", 2)

    def apply(p: LispContext): LispContext = {
      val (uri, a) = p.parameters.uriSExpr
      val r = matrix_save(p, uri, a)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object MatrixChart extends IoFunction {
    val specification = FunctionSpecification("matrix-chart", 1)

    def apply(p: LispContext): LispContext = {
      val a = p.parameters.argument1[IMatrix[Double]](specification)
      val r = matrix_chart(p, a)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object RecordMake extends ApplyFunction {
    val specification = FunctionSpecification("record-make", 1)

    def apply(u: LispContext): LispContext = {
      val a = normalize_auto_urx(u, u.parameters.head)
      val r = record_make(u, a)
      u.toResult(r)
    }
  }

  case object TableLoad extends IoFunction {
    val specification = FunctionSpecification("table-load", 1)

    def apply(u: LispContext): LispContext = {
      val a = u.parameters.argument1[URI](specification)
      val r = table_load(u, a)
      u.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object TableSave extends IoFunction {
    val specification = FunctionSpecification("table-save", 2)

    def apply(u: LispContext): LispContext = {
      val (uri, a) = u.parameters.uriSExpr
      val r = table_save(u, uri, a)
      u.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object TableMake extends ApplyFunction {
    val specification = FunctionSpecification("table-make", 1)

    def apply(u: LispContext): LispContext = {
      val a = normalize_auto_urx(u, u.parameters.head)
      val caption = u.parameters.properties.get('caption)
      val xpathpred = _xpathpred(caption)
      val strategy = u.parameters.properties.get('strategy).map { x =>
        CreateHtmlStrategy.get(x).getOrElse(RAISE.invalidArgumentFault(s"Unkown strategy: $x"))
      }.getOrElse(CreateHtmlStrategy.NaturalStrategy)
      val r = table_make(u, a, xpathpred, strategy)
      u.toResult(r)
    }

    private def _xpathpred(caption: Option[SExpr]): Option[XPathPredicate] =
      caption.map(x =>
        XPathPredicate(XPathPredicate.ChildElementContainText("caption", x.asString))
      )
  }

  case object TableMatrix extends ParameterEvalFunction {
    val specification = FunctionSpecification("table-matrix", 1)

    def eval(p: Parameters): SExpr = {
      val t = p.argument1[ITable](specification)
      val range = p.getArgumentOneBased(2) map {
        case m: SRange => m
        case m: SInterval => m.toRange
        case m: SNumber => m.toRange
        case m => RAISE.syntaxErrorFault(s"No range: ${m}")
      }
      range.map(table_matrix(t, _)).getOrElse(table_matrix(t))
    }
  }

  case object TableSelect extends ApplyFunction {
    val specification = FunctionSpecification("table-select", 2)

    def apply(u: LispContext): LispContext = {
      val p = u.parameters
      case class Z( // TODO use Parameters.Cursor. Need to update Cursor feature.
        tables: Vector[STable] = Vector.empty,
        xs: Vector[Either[String, Int]] = Vector.empty
      ) {
        lazy val table: STable = tables.headOption.map(x =>
          if (tables.tail.isEmpty)
            x
          else
            RAISE.invalidArgumentFault(s"Too many tables.")
        ).getOrElse(RAISE.invalidArgumentFault(s"No table."))

        def indexes: Seq[Int] = xs map {
          case Right(i) => i
          case Left(s) => _to_index(s)
        }

        private def _to_index(name: String) = table.schema.columns.indexWhere(_.name == name)

        def +(rhs: SExpr) = rhs match {
          case m: STable => copy(tables = tables :+ m)
          case m: SUrl => copy(tables = tables :+ table_load(u, m))
          case m: SUri => copy(tables = tables :+ table_load(u, m))
          case m: SRange => _add(m.range.indexes)
          case m: SInterval => _add(m.interval.toRange.indexes)
          case m: SNumber => _add(m.asInt)
          case m: SString => _add(m.string)
          case m: SAtom => _add(m.name)
          case m => RAISE.invalidArgumentFault(s"No index: ${m}")
        }

        private def _add(p: Int) = copy(xs = xs :+ Right(p))
        private def _add(ps: Seq[Int]) = copy(xs = xs ++ ps.map(Right(_)))
        private def _add(p: String) = copy(xs = xs :+ Left(p))
      }
      val z = p.arguments./:(Z())(_+_)
      val r = table_select(z.table, z.indexes)
      u.toResult(r)
    }
  }

  case object TableSimpleRegression extends ApplyFunction {
    val specification = FunctionSpecification("table-simple-regression", 1)

    def apply(u: LispContext): LispContext = {
      val t = u.parameters.argument1[ITable](specification)
      val r = table_regression(u, t)
      u.toResult(r)
    }
  }

  case object TableChart extends IoFunction {
    val specification = FunctionSpecification("table-chart", 1)

    def apply(p: LispContext): LispContext = {
      val t = p.parameters.argument1[ITable](specification)
      val as = p.parameters.getPropertyStringList('analyzes)
      val r = table_chart(p, t, as)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  // case object TableChart extends IoFunction {
  //   val specification = FunctionSpecification("table-chart", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       table <- p.param.table(p)
  //       analyzes <- p.param.propertyStringList('analyze)
  //     } yield {
  //       (table |@| analyzes) { (t, as) =>
  //         table_chart(p, t, as)
  //       }
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  case object GenerateId extends IoFunction {
    import org.goldenport.values.CompactUuid

    val specification = FunctionSpecification("generate-id", 0)

    def apply(p: LispContext): LispContext = {
      val r = SString(CompactUuid.generateString)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }
}
