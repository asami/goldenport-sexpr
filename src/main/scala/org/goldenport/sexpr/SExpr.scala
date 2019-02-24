package org.goldenport.sexpr

import scala.concurrent.Future
import java.net.{URL, URI}
import org.joda.time._
import play.api.libs.json.{JsValue, Json}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NTemplate}
import org.goldenport.matrix.IMatrix
import org.goldenport.table.ITable
import org.goldenport.record.v3.{IRecord}
import org.goldenport.record.http.{Request, Response}
import org.goldenport.values.Urn
import org.goldenport.io.{InputSource, StringInputSource, UrlInputSource}
import org.goldenport.bag.ChunkBag
import org.goldenport.xml.dom.DomParser
import org.goldenport.sexpr.eval.{LispContext, LispFunction}

/**
 * @since   Sep.  9, 2012
 *  version Aug. 21, 2013
 *  version Jan.  9, 2014
 *  version Feb. 27, 2014
 *  version Apr. 23, 2014
 *  version May. 25, 2014
 *  version Aug.  4, 2014
 *  version Dec. 17, 2014
 *  version Mar. 11, 2015
 *  version May. 23, 2015
 *  version Aug. 19, 2018
 *  version Sep. 29, 2018
 *  version Oct. 28, 2018
 *  version Jan.  3, 2019
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait SExpr {
  def isNilOrFalse: Boolean = false
  def getList: Option[List[SExpr]] = None
  //  def asNumber: SNumber = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def getString: Option[String] = None
  def asString: String = getString getOrElse RAISE.unsupportedOperationFault(s"Not string(${this})")
  def asInt: Int = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asBigDecimal: BigDecimal = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asUrl: URL = getString.map(new URL(_)).getOrElse(RAISE.unsupportedOperationFault(s"Not URL(${this})"))
  def asUri: URI = getString.map(new URI(_)).getOrElse(RAISE.unsupportedOperationFault(s"Not URI(${this})"))
  def getInputSource: Option[InputSource] = getString.map(StringInputSource(_))
  def resolve: SExpr = this
  def print: String // interaction representation (e.g. REPL)
  def show: String // for debug
  def detail: String = show

  protected final def cut_string(p: String) = Strings.cutstring(p, 32)
}

case class SAtom(name: String) extends SExpr {
  def print = name
  def show = name
}
object SAtom {
  val quote = SAtom("quote")
}

case class SKeyword(name: String) extends SExpr {
  def print = show
  def show = ":" + name
}

case class SNumber(number: BigDecimal) extends SExpr { // XXX Rational
  override lazy val asString = number.toString
  def print = show
  def show = number.toString
//  override def asNumber = this
  override def asBigDecimal = number
}
object SNumber {
  val ZERO = SNumber(BigDecimal(0))
  val ONE = SNumber(BigDecimal(1))

  def apply(p: String): SExpr = SNumber(BigDecimal(p))
  def apply(p: Int): SExpr = p match {
    case 0 => ZERO
    case 1 => ONE
    case _ => SNumber(BigDecimal(p))
  }
  def apply(p: Long): SExpr = p match {
    case 0 => ZERO
    case 1 => ONE
    case _ => SNumber(BigDecimal(p))
  }
  def apply(p: Float): SExpr = p match {
    case 0 => ZERO
    case 1 => ONE
    case _ => SNumber(BigDecimal(p))
  }
  def apply(p: Double): SExpr = p match {
    case 0 => ZERO
    case 1 => ONE
    case _ => SNumber(BigDecimal(p))
  }
}

case class SRational(number: spire.math.Rational) extends SExpr {
  override lazy val asString = number.toString
  def print = show
  def show = number.toString
}

case class SComplex(number: spire.math.Complex[BigDecimal]) extends SExpr {
  override lazy val asString = number.toString
  def print = show
  def show = number.toString
}

case class SBoolean(value: Boolean) extends SExpr {
  override def isNilOrFalse = !value
  def print = show
  def show = value.toString
}

object SBoolean {
  val TRUE = SBoolean(true)
  val FALSE = SBoolean(false)

  def create(p: Boolean) = if (p) TRUE else FALSE

  def isTrue(s: SExpr): Boolean = {
    s match {
      case SBoolean(false) => false
      case SNil => false
      case _ => true
    }
  }
}

case class SString(string: String) extends SExpr {
  override def toString() = s"SString($show)"
  override def getString = Some(string)
  override def asString = string
  lazy val print = string
  lazy val show = SExpr.toStringLiteral(cut_string(string))
}

sealed trait SList extends SExpr {
  override def isNilOrFalse = list.isEmpty
  lazy val length = vector.length
  lazy val list: List[SExpr] = Nil
  lazy val vector: Vector[SExpr] = Vector.empty
  def append(p: SList): SList
  def append(p: List[SExpr]): SList
  def print = list.map(_.show).mkString("(", " ", ")")
  def show = list.map(_.show).mkString("(", " ", ")")
}

object SList {
  def apply(xs: SExpr*): SList = create(xs)

  def create(xs: Seq[SExpr]): SList = {
    xs.foldRight(SNil: SList) { (x, z) =>
      SCell(x, z)
    }
  }
}

case class SCell(car: SExpr, cdr: SExpr) extends SList {
  override lazy val getList: Option[List[SExpr]] = Some(list)
  override lazy val list: List[SExpr] = SExpr.build(this)
  override lazy val vector: Vector[SExpr] = SExpr.buildVector(this)
  def append(p: SList) = SList.create(vector ++ p.vector)
  def append(p: List[SExpr]) = SList.create(list ++ p)
  override def print = cdr match {
    case _: SList => super.print
    case x => s"(${car.print} . ${x.print})"
  }
  override def show = cdr match {
    case _: SList => super.show
    case x => s"(${car.show} . ${x.show})"
  }
}

case object SNil extends SList {
  def append(p: SList) = p
  def append(p: List[SExpr]) = SList.create(p)
}

case class SError(
  label: Option[String],
  exception: Option[Throwable],
  request: Option[Request],
  response: Option[Response]
) extends SExpr {
  def message = label orElse exception.map(_.toString) orElse response.map(_.show) orElse request.map(_.show) getOrElse ""
  def print = show
  lazy val show = s"Error($message)"
}
object SError {
  def apply(p: String): SError = SError(Some(p), None, None, None)
  def apply(p: Throwable): SError = SError(None, Some(p), None, None)
  def apply(req: Request, e: Throwable): SError = SError(None, Some(e), Some(req), None)
  def apply(req: Request, res: Response): SError = SError(None, None, Some(req), Some(res))
  def apply(p: Response): SError = SError(None, None, None, Some(p))
  def functionNotFound(name: String): SError = {
    val label = s"Function '$name' is not found."
    SError(Some(label), None, None, None)
  }
}

case class SMetaCommand(command: String, args: List[String]) extends SExpr {
  def print = show
  def show = toString
}
object SMetaCommand {
  def apply(p: String): SMetaCommand = Strings.totokens(p) match {
    case Nil => SMetaCommand("help", Nil)
    case x :: xs => SMetaCommand(x, xs)
  }
}

trait IDocument extends IRecord
trait IProcess
// trait IMatrix
// trait IPath
// trait IScript

case class SBinary(binary: ChunkBag) extends SExpr {
  def print = RAISE.noReachDefect
  lazy val show = s"Binary(${binary.size})"
}

case class SI18NString(string: I18NString) extends SExpr {
  def print = show
  def show = string.toString
}

case class SI18NTemplate(template: I18NTemplate) extends SExpr {
  def print = show
  def show = template.toString
}

case class SRegex(regex: scala.util.matching.Regex) extends SExpr {
  override def equals(rhs: Any): Boolean = rhs match {
    case SRegex(r) => regex.toString == r.toString
    case _ => false
  }
  def print = show
  def show = regex.toString
}

case class SBag(bag: ChunkBag) extends SExpr {
  def print = bag.toText
  def show = "Bag()"
}

case class SDocument(document: IDocument) extends SExpr { // Bean
  def print = show
  def show = document.toString
}

case class SRecord(record: IRecord) extends SExpr {
  def print = show
  def show = record.toString
}

case class STable(table: ITable) extends SExpr {
  def print = show
  def show = table.toString
}

case class SMatrix(matrix: IMatrix[Double]) extends SExpr {
  def print = show
  def show = matrix.toString
}

case class SUrl(url: java.net.URL) extends SExpr {
  override lazy val asString = url.toString
  override def asUrl = url
  override def asUri = url.toURI
  override def getInputSource = Some(UrlInputSource(url))
  def print = show
  def show = url.toString
}

case class SUrn(urn: Urn) extends SExpr {
  override lazy val asString = urn.toString
  override def asUri = urn.toURI
  def print = show
  def show = urn.toString
}

// ScriptEngine, JEXL
case class SScript(language: Option[String], text: String) extends SExpr {
  def print = show
  def show = text
}
object SScript {
  def apply(p: String): SScript = SScript(None, p)
}

case class SProcess(process: IProcess) extends SExpr {
  def print = show
  def show = toString
}

case class SSingleQuote() extends SExpr {
  def print = show
  def show = "SingleQuote()"
}

case class SObject(o: AnyRef) extends SExpr {
  def print = show
  def show = o.toString
}

case class SXml(text: String) extends SExpr {
  def print = text
  def show = s"Xml(${cut_string(text)})"
  lazy val dom = DomParser.parse(text)
}
object SXml {
//  def apply(p: String): SXml = ???
}

case class SHtml(html: String) extends SExpr { // TODO nekohtml
  override def asString = html
  def print = show
  def show = html
  lazy val dom = ???
  lazy val nekodom = ???
}

// JXPath
case class SXPath(path: String) extends SExpr {
  override def asString = path
  def print = path
  def show = s"XPath($path)"
}

case class SXslt(xslt: String) extends SExpr {
  override def asString = xslt
  def print = xslt
  def show = s"XSLT(${cut_string(xslt)})"
  lazy val dom: org.w3c.dom.Node = ???
}

case class SPug(pug: String) extends SExpr {
  override def asString = pug
  def print = pug
  def show = s"Pug(${cut_string(pug)})"
  lazy val dom = ???
  lazy val nekodom = ???
}

case class SJson(text: String) extends SExpr {
  override def asString = text
  def print = text
  def show = s"Json(${cut_string(text)})"
  lazy val json = Json.parse(text)
}
object SJson {
}

case class SDateTime(datetime: DateTime) extends SExpr {
  def print = show
  def show = datetime.toString
}

case class SLocalDateTime(datetime: LocalDateTime) extends SExpr {
  def print = show
  def show = datetime.toString
}

case class SLocalDate(date: LocalDate) extends SExpr {
  def print = show
  def show = date.toString
}

case class SLocalTime(time: LocalTime) extends SExpr {
  def print = show
  def show = time.toString
}

case class SMonthDay(monthday: MonthDay) extends SExpr {
  def print = show
  def show = monthday.toString // ISO8601 (e.g. --05-06)

  def month: Int = monthday.getMonthOfYear
  def day: Int = monthday.getDayOfMonth
}

case class SInterval(interval: Interval) extends SExpr {
  def print = show
  def show = interval.toString
}

case class SDuration(duration: Duration) extends SExpr {
  def print = show
  def show = duration.toString
}

case class SPeriod(period: Period) extends SExpr {
  def print = show
  def show = period.toString
}

case class SCurrency(currency: BigDecimal) extends SExpr {
  def print = show
  def show = currency.toString
}

case class SPercent(percent: BigDecimal) extends SExpr {
  def print = show
  def show = percent.toString
}

case class SUnit(unit: String) extends SExpr {
  def print = show
  def show = unit
}

trait SExtension extends SExpr {
}

trait SControl extends SExpr {
  override lazy val resolve = resolveContext.value
  def resolveContext: LispContext
}

case class SFuture(c: LispContext, f: LispFunction) extends SControl {
  def print = show // TODO
  def show = s"SFuture(${f.name})"
  private lazy val _future = c.futureForEval(f)
  def start(): SFuture = {
    _future
    this
  }
  def resolveContext: LispContext = c.wait(_future)
}

case class SLazy(c: LispContext, f: LispFunction) extends SControl {
  def print = show // TODO
  def show = s"SLazy(${f.name})"
  def resolveContext: LispContext = f(c)
}

case class SLazyFuture(label: String) extends SControl {
  def print = show // TODO
  def show = s"SLazyFuture($label)"
  def resolveContext: LispContext = RAISE.notImplementedYetDefect
}

case class SWait(label: String, body: () => LispContext) extends SControl {
  def print = show // TODO
  def show = s"SWait($label)"
  def resolveContext: LispContext = body()
}

case class SFutureWait(label: String, c: LispContext, body: () => LispContext) extends SControl {
  def print = show // TODO
  def show = s"SFutureWait($label)"
  private lazy val _future = c.future(label, body)
  def start(): SFutureWait = {
    _future
    this
  }
  def resolveContext: LispContext = c.wait(_future)
}
object SFutureWait {
  def create(label: String, c: LispContext)(body: => LispContext): SFutureWait =
    SFutureWait(label, c, () => body)
}

trait SPseudo extends SExpr

case object SOpen extends SPseudo { // List Open
  def print = show
  def show = "#open"
}
case object SClose extends SPseudo { // List Close
  def print = show
  def show = "#close"
}
case object SSpace extends SPseudo { // Space
  def print = show
  def show = "#space"
}
case object SDelimiter extends SPseudo { // Special delimiter ','
  def print = show
  def show = "#delimier"
}

object SExpr {
  def create(p: Any): SExpr = p match {
    case m: SExpr => m
    case m: Boolean => SBoolean.create(m)
    case m: Byte => SNumber(m)
    case m: Short => SNumber(m)
    case m: Int => SNumber(m)
    case m: Long => SNumber(m)
    case m: Float => SNumber(m)
    case m: Double => SNumber(m)
    case m: String => SString(m)
    case m: AnyRef => SObject(m) // TODO
  }

  def getKeyword[T](expr: SExpr, keyword: String)(implicit pf: PartialFunction[SExpr, T]): Option[T] = {
    expr.getList.flatMap(_.dropWhile(_ match {
        case k: SKeyword if k.name == keyword => false
        case _ => true
      }) match {
        case x :: Nil => None
        case x :: xs => {
          pf.lift(xs.head)
        }
        case Nil => None
    })
  }

  def build(cell: SCell): List[SExpr] = {
    val buf = new scala.collection.mutable.ListBuffer[SExpr]
    var c = cell
    do { 
      buf += c.car
      c.cdr match {
        case SNil => c = null
        case x: SCell => c = x
        case _ => c = null
      }
    } while (c != null)
    buf.toList
  }

  def buildVector(cell: SCell): Vector[SExpr] = {
    val buf = new scala.collection.mutable.ArrayBuffer[SExpr]
    var c = cell
    do { 
      buf += c.car
      c.cdr match {
        case SNil => c = null
        case x: SCell => c = x
        case _ => c = null
      }
    } while (c != null)
    buf.toVector
  }

  // sync with SExprParser#string
  def toStringLiteral(s: String): String = {
    val a =
      if (s.contains('\n') | s.contains('\r') | s.contains('\t') |
        s.contains('\"') | s.contains('\\')
      ) {
        s.replace("\\", "\\\\").
          replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t").
          replace("\"", "\\\"")
      } else {
        s
      }
    "\"" + a + "\""
  }

  object Implicits {
    implicit object SExprToSExpr extends PartialFunction[SExpr, SExpr] {
      def isDefinedAt(s: SExpr): Boolean = true
      def apply(s: SExpr): SExpr = s
    }

    implicit object SExprToString extends PartialFunction[SExpr, String] {
      def isDefinedAt(s: SExpr): Boolean = {
        s match {
          case _: SString => true
          case _ => false
        }
      }
      def apply(s: SExpr): String = {
        (s: @unchecked) match {
          case s: SString => s.string
        }
      }
    }

    implicit object SExprToSExprList extends PartialFunction[SExpr, List[SExpr]] {
      def isDefinedAt(s: SExpr): Boolean = {
        s match {
          case _: SCell => true
          case _ => false
        }
      }
      def apply(s: SExpr): List[SExpr] = {
        s.getList.get
      }
    }

    implicit object SExprToStringList extends PartialFunction[SExpr, List[String]] {
      def isDefinedAt(s: SExpr): Boolean = {
        s match {
          case _: SCell => true
          case _ => false
        }
      }
      def apply(s: SExpr): List[String] = {
        s.getList match {
          case Some(xs) => xs.collect {
            case s: SString => s.string
          }
          case _ => Nil
        }
      }
    }
  }
}
