package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import java.io.File
import java.net.{URL, URI}
import play.api.libs.json._
import org.goldenport.Strings
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.{Record, ITable}
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.http.{Request, Response}
import org.goldenport.io.{MimeType, UrlUtils, Retry => LibRetry}
import org.goldenport.matrix.{IMatrix, Matrix}
import org.goldenport.bag.{EmptyBag, ChunkBag, StringBag}
import org.goldenport.xml.dom.DomUtils
import org.goldenport.log.Loggable
import org.goldenport.cli.ShellCommand
import org.goldenport.incident.{Incident => LibIncident}
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
 * @version Dec.  2, 2019
 * @author  ASAMI, Tomoharu
 */
trait LispFunction extends PartialFunction[LispContext, LispContext]
    with UtilityPart with MatrixPart with XPathPart with RecordPart with TablePart with Loggable {
  def specification: FunctionSpecification
  def name: String = specification.name

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

  protected final def to_string(u: LispContext, p: SExpr): String = u.formatString(p)

  protected final def string_interpolate(u: LispContext, ps: Seq[SExpr]): SExpr =
    SString(ps.map(string_interpolate_string(u, _)).mkString)

  protected final def string_interpolate_string(u: LispContext, p: SExpr): String = p match {
    case SString(s) => InterpolationParser.parse(s).map {
      case Right(r) => to_string(u, u.eval(SScript.create(r)))
      case Left(l) => l
    }.mkString
    case m => to_string(u, m)
  }

  protected final def string_concatenate(u: LispContext, ps: Seq[SExpr]): SExpr =
    SString(ps.map(to_string(u, _)).mkString)

  protected final def string_format(u: LispContext, template: String, ps: Seq[SExpr]): SExpr =
    SString(ps.map(x => u.formatString(template, x)).mkString)

  protected final def string_message(u: LispContext, template: String, ps: Seq[SExpr]): SExpr =
    SString(u.formatMessage(template, ps))

  protected final def string_message_key(u: LispContext, key: String, ps: Seq[SExpr]): SExpr =
    SString(u.formatMessageKey(key, ps))

  protected final def normalize_auto(u: LispContext, p: SExpr): SExpr = p match {
    case m: SUrl => resolve_url(u, m)
    case m: SUrn => resolve_urn(u, m)
    case _ => SExpr.normalizeAuto(p)
  }

  protected final def resolve_url(u: LispContext, url: SUrl): SExpr = {
    val start = System.currentTimeMillis
    try {
      val res = u.serviceLogic.fileFetch(url.url)
      log_debug(s"resolve_url: $url => $res")
      val i = FileIncident(start, url, res)
      response_result(url, res)
    } catch {
      case NonFatal(e) =>
        log_debug(s"resolve_url: $url => $e")
        val i = FileIncident(start, url, e)
        SError(i)
    }
  }

  protected final def resolve_uri(u: LispContext, uri: URI): SExpr = {
    RAISE.notImplementedYetDefect
  }

  protected final def file_fetch(u: LispContext, url: URL): LispContext = {
    val start = System.currentTimeMillis
    try {
      val res = u.serviceLogic.fileFetch(url)
      log_debug(s"file_fetch: $url => $res")
      val i = FileIncident(start, url, res)
      val r = response_result(url, res)
      u.toResult(r, i)
    } catch {
      case NonFatal(e) =>
        log_debug(s"file_fetch: $url => $e")
        val i = FileIncident(start, url, e)
        u.toResult(SError(i), i)
    }
  }

  protected final def resolve_urn(u: LispContext, urn: SUrn): SExpr = urn // TODO driver

  protected final def response_result(url: URL, p: ChunkBag): SExpr = {
    def string = p.toText
    def stringorclob = p.getSize.map(size =>
      if (size > 8192)
        SString(p.toText)
      else
        SClob(p)
    ).getOrElse(SClob(p))
    val mime = MimeType.getBySuffix(url)
    mime.map {
      case MimeType.TEXT_XSL => SXsl(string)
      case m => 
        if (m.isHtml)
          SHtml(string)
        else if (m.isXml)
          SXml(string)
        else if (m.isJson)
          SJson(string)
        else if (m.isText)
          stringorclob
        else
          SBlob(p)
    }.getOrElse(SBlob(p))
  }
}

trait ApplyFunction extends LispFunction {
}

trait EvalFunction extends LispFunction {
  def apply(p: LispContext): LispContext = {
    val r = eval(p.parameters)
    p.toResult(r)
  }

  def eval(p: Parameters): SExpr
}

trait ResolvedParametersFeature { self: EvalFunction =>
  override def apply(p: LispContext): LispContext = {
    val ps = p.parameters.map(normalize_auto(p, _))
    val r = eval(ps)
    p.toResult(r)
  }
}

trait ControlFunction extends LispFunction {
}

trait HeavyFunction extends LispFunction { // CPU bound
}

trait EffectFunction extends LispFunction {
  def applyEffect(p: LispContext): UnitOfWorkFM[LispContext]
}

trait IoFunction extends EffectFunction { // I/O bound
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
    val result = new ShellCommand(commands, env, dir, is, timeout).run
    SWait(name, { () =>
      val code = result.waitFor
      // println(s"execute_shell_command: $commands => $code")
      if (code == 0)
        _success(u, result)
      else
        _failure(u, result)
    })
  }

  private def _success(c: LispContext, p: ShellCommand.Result) = {
    val (props, stdout) = _properties(p)
    c.toResult(stdout, props)
  }

  private def _failure(c: LispContext, p: ShellCommand.Result) = {
    val (props, _) = _properties(p)
    val error = SError(s"Shell Command: ${p.waitFor}")
    c.toResult(error, props)
  }

  private def _properties(p: ShellCommand.Result): (Record, SBlob) = {
    val retval = SNumber(p.waitFor)
    val stdout = SBlob(p.stdout)
    val stderr = SBlob(p.stderr)
    val a = Record.data(
      "return-code" -> retval,
      "stdout" -> stdout,
      "stderr" -> stderr
    )
    (a, stdout)
  }

  protected final def is_implicit_http_communication(u: LispContext): Boolean =
    u.bindings.getUrl("http.baseurl").isDefined

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
      else if (res.isNotFound)
        u.toResult(SError.functionNotFound(functionname)).
          withUnavailableFunction(functionname)
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
}

trait AsyncIoFunction extends IoFunction { // I/O bound, implicit asynchronous in function
}

trait SyncIoFunction extends IoFunction { // I/O bound, synchronous is required.
}

object LispFunction {
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

  case object Car extends EvalFunction {
    val specification = FunctionSpecification("car", 1)
    def eval(p: Parameters) =
      p.arguments.headOption.map {
        case m: SCell => m.car
        case m => SError(s"Not list: $m")
      }.getOrElse(SError("Empty list"))
  }

  case object Cdr extends EvalFunction {
    val specification = FunctionSpecification("cdr", 1)
    def eval(p: Parameters) =
      p.arguments.headOption.map {
        case m: SCell => m.cdr
        case m => SError(s"Not list: $m")
      }.getOrElse(SError("Empty list"))
  }

  case object And extends EvalFunction {
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

  case object Or extends EvalFunction {
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

  case object Plus extends EvalFunction {
    val specification = FunctionSpecification("+", 2)

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

  case object Minus extends EvalFunction {
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

  case object Multify extends EvalFunction {
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

  case object Divide extends EvalFunction {
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

  case object Length extends EvalFunction {
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

  case object Inv extends EvalFunction {
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

  case object Pop extends LispFunction {
    val specification = FunctionSpecification("pop")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      params.getArgument1[Int](specification).map(x => p.pop(x)).getOrElse(p.pop)
    }
  }

  case object Peek extends LispFunction {
    val specification = FunctionSpecification("peek")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      val r = params.getArgument1[Int](specification).map(x => p.peek(x)).getOrElse(p.peek)
      p.toResult(r)
    }
  }

  case object Mute extends EvalFunction {
    val specification = FunctionSpecification("mute")
    def eval(p: Parameters) = SMute(p.head)
  }

  case object History extends LispFunction {
    val specification = FunctionSpecification("history")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      val r = params.getArgument1[Int](specification).map(x => p.takeHistory(x)).getOrElse(p.takeHistory(1))
      p.toResult(r)
    }
  }

  // TODO
  case object CommandHistory extends LispFunction { // XXX call, invoke, request, command ?
    val specification = FunctionSpecification("command-history")
    def apply(p: LispContext): LispContext = {
      val params = p.parameters
      val r = params.getArgument1[Int](specification).map(x => p.takeCommandHistory(x)).getOrElse(p.takeCommandHistory)
      p.toResult(r)
    }
  }

  case object PathGet extends EvalFunction with ResolvedParametersFeature {
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
    }

    // override def isDefinedAt(p: LispContext): Boolean =
    //   // p.value.isInstanceOf[SXPath] || is_defined_at(p)
    //   is_defined_at(p)

    def eval(p: Parameters) = {
      val returntype: ReturnType = ReturnType(p.getPropertyString('type))
      val (xpath, x) = p.arguments(0) match {
        case m: SXPath => (m, p.arguments(1))
        case m => p.arguments(1) match {
          case mm: SXPath => (mm, m)
          case mm => RAISE.syntaxErrorFault(s"No xpath both ${m} and ${mm}")
        }
      }
      val target = x
      _traverse(returntype, xpath, target)
    }

    def traverse(rtype: ReturnType, xpath: SXPath, target: SExpr): SExpr =
      _traverse(rtype, xpath, target)

    private def _traverse(rtype: ReturnType, xpath: SXPath, target: SExpr): SExpr = try {
      target match {
        case m: SXml => _traverse_xml(rtype, xpath, m)
        case m: SHtml => _traverse_html(rtype, xpath, m)
        case m: SJson => _traverse_json(rtype, xpath, m)
        case m: SRecord => _traverse_record(rtype, xpath, m)
        case m: STable => _traverse_table(rtype, xpath, m)
        case m => _traverse_bean(rtype, xpath, m) // SError.syntaxError(s"Unaviable for xpath: $m")
      }
    } catch {
      case NonFatal(e) => SError(e)
    }

    private def _traverse_xml(rtype: ReturnType, xpath: SXPath, p: SXml) =
      _traverse_dom_xpath(rtype, xpath, p.dom)

    private def _traverse_dom_xpath(
      rtype: ReturnType,
      xpath: SXPath,
      p: org.w3c.dom.Node
    ) = {
      val engine = XPathFactory.newInstance().newXPath()
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
              if (DomUtils.isTextOnly(xs))
                xs.map(_.getTextContent).mkString
              else if (xs.forall(DomUtils.isTextOnlyChildren))
                xs.map(_.getTextContent)
              else
                m
            case _ => a
          }
        } else {
          a
        }
      )
      SExpr.createOrNil(x)
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
      _traverse_dom_xpath(rtype, xpath, p.dom)

    private def _traverse_json(rtype: ReturnType, xpath: SXPath, p: SJson) = {
      val pc = JXPathContext.newContext(p.json)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse_record(rtype: ReturnType, xpath: SXPath, p: SRecord) = {
      // val pc = JXPathContext.newContext(p.asObject)
      val pc = RecordJxPathContext.newContext(p.record)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse_table(rtype: ReturnType, xpath: SXPath, p: STable) = {
      val pc = JXPathContext.newContext(p.table)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse_bean(rtype: ReturnType, xpath: SXPath, p: SExpr) = {
      val pc = JXPathContext.newContext(p.asObject)
      _traverse(rtype, xpath, pc)
    }

    private def _traverse(rtype: ReturnType, xpath: SXPath, pc: JXPathContext): SExpr = {
      def v0 = rtype.xpath match {
        case XPathConstants.BOOLEAN => pc.getValue(xpath.path, classOf[Boolean])
        case XPathConstants.NUMBER => pc.getValue(xpath.path, classOf[BigDecimal])
        case XPathConstants.STRING => rtype.datatype.toInstance(pc.getValue(xpath.path, classOf[String]))
        case XPathConstants.NODE => pc.getValue(xpath.path)
        case XPathConstants.NODESET => pc.getValue(xpath.path)
      }

      val v = if (rtype.auto) {
        pc.iterate(xpath.path).asScala.toList match {
          case Nil => SNil
          case x :: Nil => x
          case xs => xs
        }
      } else {
        v0
      }
      SExpr.create(v)
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

  case object Transform extends EvalFunction {
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

  case object Xslt extends LispFunction {
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
      val r = DomUtils.transform(xslt.xslt, p.dom) // Caution: xslt.dom doen't work.
      SXml(r)
    }
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
          SExpr.create(s, r)
        }
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r)
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

  case object VectorVerticalFill extends EvalFunction {
    val specification = FunctionSpecification("vector-vertical-fill", 2)

    def eval(p: Parameters) = {
      val (count, v) = p.argument2[Int, Double](specification)
      SMatrix(Matrix.vectorVerticalFill(count, v))
    }
  }

  case object MatrixHorizontalConcatenate extends EvalFunction {
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
      val a = normalize_auto(u, u.parameters.head)
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

  case object TableMake extends ApplyFunction {
    val specification = FunctionSpecification("table-make", 1)

    def apply(u: LispContext): LispContext = {
      val a = normalize_auto(u, u.parameters.head)
      val r = table_make(u, a)
      u.toResult(r)
    }
  }

  case object TableMatrix extends EvalFunction {
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

  case object TableChart extends IoFunction {
    val specification = FunctionSpecification("table-chart", 1)

    def apply(u: LispContext): LispContext = {
      val a = u.parameters.argument1[ITable](specification)
      val r = table_chart(u, a)
      u.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }
}
