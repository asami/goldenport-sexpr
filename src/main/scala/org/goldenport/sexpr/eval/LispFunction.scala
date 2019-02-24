package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.net.{URL, URI}
import org.goldenport.exception.RAISE
import org.goldenport.record.v3.Record
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.http.{Request, Response}
import org.goldenport.io.MimeType
import org.goldenport.sexpr._, SExprConverter._
import org.goldenport.matrix.IMatrix
import org.goldenport.table.ITable

/*
 * @since   Sep. 10, 2018
 *  version Sep. 29, 2018
 *  version Oct. 30, 2018
 *  version Jan.  3, 2019
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
trait LispFunction extends PartialFunction[LispContext, LispContext]
    with MatrixPart with TablePart {
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
}

trait EvalFunction extends LispFunction {
  def apply(p: LispContext): LispContext = {
    val r = eval(p.parameters)
    p.toResult(r)
  }

  def eval(p: Parameters): SExpr
}

trait ControlFunction extends LispFunction {
}

trait HeavyFunction extends LispFunction { // CPU bound
}

trait EffectFunction extends LispFunction {
  def applyEffect(p: LispContext): UnitOfWorkFM[LispContext]
}

trait IoFunction extends EffectFunction { // I/O bound
  protected final def http_get(u: LispContext): LispContext =
    http_call(Request.GET, u)

  protected final def http_post(u: LispContext): LispContext =
    http_call(Request.POST, u)

  protected final def http_put(u: LispContext): LispContext =
    http_call(Request.PUT, u)

  protected final def http_delete(u: LispContext): LispContext =
    http_call(Request.DELETE, u)

  protected final def http_call(method: Request.Method, u: LispContext): LispContext = {
    val req = build_request(method, u)
    try {
      val res = u.serviceLogic.httpService(req)
      if (res.isSuccess)
        u.toResult(res)
      else
        u.toResult(SError(req, res))
    } catch {
      case NonFatal(e) => u.toResult(SError(req, e))
    }
  }

  protected final def build_request(
    method: Request.Method,
    p: LispContext
  ): Request = build_request(
    method,
    p.parameters,
    p.bindings.getUrl("http.baseurl"),
    p.bindings.getStringList("http.header")
  )

  protected final def build_request(
    method: Request.Method,
    parameters: Parameters,
    baseurl: Option[URL],
    header: Option[List[String]]
  ): Request = {
    val uri = parameters.arguments1[URI](specification)
    val query = parameters.getProperty('query).map(build_request_record)
    val form = parameters.getProperty('form).map(build_request_record)
    val url = try {
      uri.toURL
    } catch {
      case NonFatal(e) => baseurl.
          map(new URL(_, uri.toASCIIString)).
          getOrElse(RAISE.invalidArgumentFault(s"Unresolved uri: $uri"))
    }
    val h = _build_header(header)
    Request(url, method, query.orZero, form.orZero, h)
  }

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
    p.map(x =>
      RAISE.notImplementedYetDefect(this, s"$x")
    ).getOrElse(Record.empty)
}

// trait AsyncIoFunction extends IoFunction { // I/O bound, implicit asyncrnouse
// }

object LispFunction {
  case object Car extends EvalFunction {
    val specification = FunctionSpecification("car", 1)
    def eval(p: Parameters) = p.arguments.head
  }

  case object Quote extends ControlFunction {
    val specification = FunctionSpecification("quote", 1)
    def apply(p: LispContext) = p.toResult(p.parameters.arguments.head)
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
    val specification = FunctionSpecification("+")
    def eval(p: Parameters) = {
      SNumber(p.asBigDecimalList.sum)
    }
  }

  case object Length extends EvalFunction {
    val specification = FunctionSpecification("length", 1)
    def eval(p: Parameters) = {
      val r = p.arguments.map {
        case m: SString => m.string.length
        case m => m.asString.length
      }.sum
      SNumber(r)
    }
  }

  case object Pop extends LispFunction {
    val specification = FunctionSpecification("pop")
    def apply(p: LispContext): LispContext = p.pop
  }

  case object PathGet extends EvalFunction {
    import org.apache.commons.jxpath._
    val specification = FunctionSpecification("pathget")

    override def isDefinedAt(p: LispContext): Boolean =
      // p.value.isInstanceOf[SXPath] || is_defined_at(p)
      is_defined_at(p)

    def eval(p: Parameters) = {
      val (xpath, target) = p.arguments(0) match {
        case m: SXPath => (m, p.arguments(1))
        case m => p.arguments(1) match {
          case mm: SXPath => (mm, m)
          case mm => RAISE.syntaxErrorFault(s"No xpath both ${m} and ${mm}")
        }
      }
      _traverse(xpath, target)
    }

    private def _traverse(xpath: SXPath, target: SExpr): SExpr = {
      target match {
        case m: SXml => _traverse_xml(xpath, m)
        case m: SJson => _traverse_json(xpath, m)
        case m: SRecord => _traverse_record(xpath, m)
        case m => RAISE.unsupportedOperationFault(m.show)
      }
    }

    private def _traverse_xml(xpath: SXPath, p: SXml) = {
      val pc = JXPathContext.newContext(p.dom)
      _traverse(xpath, pc)
    }

    private def _traverse_json(xpath: SXPath, p: SJson) = {
      val pc = JXPathContext.newContext(p.json)
      _traverse(xpath, pc)
    }

    private def _traverse_record(xpath: SXPath, p: SRecord) = {
      val pc = JXPathContext.newContext(p.record)
      _traverse(xpath, pc)
    }

    private def _traverse(xpath: SXPath, pc: JXPathContext): SExpr =
      _traverse(xpath.path, pc)

    private def _traverse(xpath: String, pc: JXPathContext): SExpr = {
      // println(s"xpath: $xpath")
      // println(s"JXPathContext: ${pc.getContextBean}")
      try {
        val v = pc.getValue(xpath)
        _to_sexpr(v)
      } catch {
        case NonFatal(e) => SError(e)
      }
    }

    private def _to_sexpr(p: Any): SExpr = p match {
      case m: String => SString(m)
      case m => RAISE.notImplementedYetDefect
    }
  }

  case object Transform extends EvalFunction {
    val specification = FunctionSpecification("transform")

    override def isDefinedAt(p: LispContext): Boolean =
      p.value.isInstanceOf[SXslt] || is_defined_at(p)

    def eval(p: Parameters) = {
      RAISE.notImplementedYetDefect
    }
  }

  case object Fetch extends IoFunction {
    val specification = FunctionSpecification("fetch", 1)

    override def isDefinedAt(p: LispContext): Boolean =
      p.value.isInstanceOf[SUrl] || is_defined_at(p)

    def apply(p: LispContext): LispContext = {
      val url = p.parameters.arguments1[URL](specification) // TODO file
      HttpGet.apply(p)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpGet extends IoFunction {
    val specification = FunctionSpecification("http-get")

    def apply(p: LispContext): LispContext = http_get(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpPost extends IoFunction {
    val specification = FunctionSpecification("http-post")

    def apply(p: LispContext): LispContext = http_post(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpPut extends IoFunction {
    val specification = FunctionSpecification("http-put")

    def apply(p: LispContext): LispContext = http_put(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object HttpDelete extends IoFunction {
    val specification = FunctionSpecification("http-delete")

    def apply(p: LispContext): LispContext = http_delete(p)

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object MatrixLoad extends IoFunction {
    val specification = FunctionSpecification("matrix-load", 1)

    def apply(p: LispContext): LispContext = {
      val a = p.parameters.arguments1[URI](specification)
      val r = matrix_load(p, a)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object MatrixChart extends IoFunction {
    val specification = FunctionSpecification("matrix-chart", 1)

    def apply(p: LispContext): LispContext = {
      val a = p.parameters.arguments1[IMatrix[Double]](specification)
      val r = matrix_chart(p, a)
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object TableLoad extends IoFunction {
    val specification = FunctionSpecification("table-load", 1)

    def apply(u: LispContext): LispContext = {
      val a = u.parameters.arguments1[URI](specification)
      val r = table_load(u, a)
      u.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object TableChart extends IoFunction {
    val specification = FunctionSpecification("table-chart", 1)

    def apply(u: LispContext): LispContext = {
      val a = u.parameters.arguments1[ITable](specification)
      val r = table_chart(u, a)
      u.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }
}
