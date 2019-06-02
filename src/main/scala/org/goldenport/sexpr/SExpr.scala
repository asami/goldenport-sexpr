package org.goldenport.sexpr

import scalaz.NonEmptyList
import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration.Duration
import java.net.{URL, URI}
import org.joda.time._
import play.api.libs.json._
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.i18n.{I18NString, I18NTemplate}
import org.goldenport.collection.NonEmptyVector
import org.goldenport.matrix.IMatrix
import org.goldenport.table.ITable
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.{IRecord}
import org.goldenport.record.http.{Request, Response}
import org.goldenport.record.store.Query
import org.goldenport.value._
import org.goldenport.values.Urn
import org.goldenport.io.{InputSource, StringInputSource, UrlInputSource}
import org.goldenport.bag.ChunkBag
import org.goldenport.xml.dom.{DomParser, DomUtils}
import org.goldenport.util.StringUtils
import org.goldenport.sexpr.eval.{LispContext, LispFunction, Incident, RestIncident}
import org.goldenport.sexpr.eval.chart.Chart

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
 *  version Feb. 24, 2019
 *  version Mar. 30, 2019
 *  version Apr. 20, 2019
 * @version May. 21, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait SExpr {
  override def toString(): String = show
  def isNilOrFalse: Boolean = false
  def getList: Option[List[SExpr]] = None
  //  def asNumber: SNumber = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def getString: Option[String] = None
  def asObject: Any = asString
  def asString: String = getString getOrElse RAISE.unsupportedOperationFault(s"Not string(${this})")
  def asInt: Int = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asLong: Long = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asFloat: Float = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asDouble: Double = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asBigDecimal: BigDecimal = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def asUrl: URL = getString.map(new URL(_)).getOrElse(RAISE.unsupportedOperationFault(s"Not URL(${this})"))
  def asUri: URI = getString.map(new URI(_)).getOrElse(RAISE.unsupportedOperationFault(s"Not URI(${this})"))
  def getInputSource: Option[InputSource] = getString.map(StringInputSource(_))
  def resolve: SExpr = this
  def print: String = try {
    SExpr.toPrint(print_String) // 1 line. for interaction representation (e.g. REPL)
  } catch {
    case NonFatal(e) => s"${getClass.getSimpleName}#print[$e]"
  }
  protected def print_String: String = getString getOrElse title
  final def display: String = try {
    SExpr.toDisplay(display_String)
  } catch {
    case NonFatal(e) => s"${getClass.getSimpleName}#display[$e]"
  }
  protected def display_String: String = print_String
  def titleName: String = getClass.getSimpleName
  def titleInfo: String = ""
  def titleDescription: String = ""
  lazy val title = titleInfo match {
    case "" => s"${titleName}"
    case i => s"${titleName}[$i]"
  }
  lazy val longTitle: String = {
    (titleInfo, titleDescription) match {
      case ("", "") => s"${titleName}"
      case ("", d) => s"${titleName}: $d"
      case (i, "") => s"${titleName}[$i]"
      case (i, d) => s"${titleName}[$i]: $d"
    }
  }
  final def show: String = try {
    show_Content.fold(s"$longTitle")(s => s"${title}\n${SExpr.toShow(s)}")
  } catch {
    case NonFatal(e) => s"${getClass.getSimpleName}#show[$e]"
  }
  protected def show_Content: Option[String] = getString // for debug
  private lazy val _description = _full_description.toShort
  def description: SExpr.Description = _description
  def descriptionContent: Seq[String] = SExpr.toFull(getString)
  private lazy val _full_description = {
    val c = descriptionContent
    if (descriptionContent.isEmpty)
      SExpr.Description(longTitle, c)
    else
      SExpr.Description(title, c)
  }
  def fullDescription: SExpr.Description = _full_description

  // def detail: Vector[String] = detailTitle +: detailContent
  // def detailName: String = 
  // def detailTitle: String = {
  //   (detailInfo, detailDescription) match {
  //     case ("", "") => s"${detailName}"
  //     case ("", d) => s"${detailName}: $d"
  //     case (i, "") => s"${detailName}[$i]"
  //     case (i, d) => s"${detailName}[$i] $d"
  //   }
  // }
  // def detailInfo: String = ""
  // def detailDescription: String = ""
  // def detailContent: Vector[String] = getString.map(Strings.tolines).getOrElse(Vector.empty)

  def carOrRaise: SExpr = RAISE.syntaxErrorFault(s"$this")
  def cdrOrRaise: SExpr = RAISE.syntaxErrorFault(s"$this")

  protected final def cut_string(p: String) = Strings.cutstring(p, 32)
}

case class SAtom(name: String) extends SExpr {
  override def getString = Some(name)
  // def print = name
  // def show = name
}
object SAtom {
  val quote = SAtom("quote")
  val lambda = SAtom("lambda")
}

case class SKeyword(name: String) extends SExpr {
  override def getString = Some(name)
  // def print = show
  // def show = ":" + name
}

// TODO add SInt, SDouble, SBigDecimal and turn to trait.
// TODO unify SRational, SComplex as SNumber
case class SNumber(number: BigDecimal) extends SExpr {
  override def asObject = number
  override lazy val asString = number.toString
  override def getString = Some(asString)
  // def print = show
  // def show = number.toString
  override lazy val asInt: Int = number.toInt
  override lazy val asLong: Long = number.toLong
  override lazy val asFloat: Float = number.toFloat
  override lazy val asDouble: Double = number.toDouble
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
  override def getString = Some(asString)
  // def print = show
  // def show = number.toString
}

case class SComplex(number: spire.math.Complex[Double]) extends SExpr {
  override lazy val asString = number.toString
  override def getString = Some(asString)
  // def print = show
  // def show = number.toString
}

case class SBoolean(value: Boolean) extends SExpr {
  override def isNilOrFalse = !value
  override def getString = Some(value.toString)
  // def print = show
  // def show = value.toString
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
  override def toString() = s"SString($string)"
  override def getString = Some(string)
  override def asString = string
  // lazy val print = SExpr.toPrint(string)
  // lazy val show = SExpr.toShow(string) // toStringLiteral(cut_string(string))
  override def titleInfo = s"${string.length}"
}

sealed trait SList extends SExpr {
  override def isNilOrFalse = list.isEmpty
  def length = vector.length
  def list: List[SExpr] = Nil
  def vector: Vector[SExpr] = Vector.empty
  def append(p: SList): SList
  def append(p: List[SExpr]): SList
  override protected def print_String = list.map(_.print).mkString("(", " ", ")")
  override protected def show_Content = Some(list.map(_.show).mkString("(", " ", ")"))
}

object SList {
  def apply(xs: SExpr*): SList = create(xs)

  def create(xs: Seq[SExpr]): SList =
    xs.foldRight(SNil: SList) { (x, z) =>
      SCell(x, z)
    }

  def create(xs: Iterator[SExpr]): SList =
    xs.foldRight(SNil: SList) { (x, z) =>
      SCell(x, z)
    }
}

case class SCell(car: SExpr, cdr: SExpr) extends SList {
  override lazy val getList: Option[List[SExpr]] = Some(list)
  override lazy val list: List[SExpr] = SExpr.build(this)
  override lazy val vector: Vector[SExpr] = SExpr.buildVector(this)
  override def carOrRaise = car
  override def cdrOrRaise = cdr
  def append(p: SList) = SList.create(vector ++ p.vector)
  def append(p: List[SExpr]) = SList.create(list ++ p)
  override protected def print_String = cdr match {
    case _: SList => super.print_String
    case x => s"(${car.print} . ${x.print})"
  }
  override protected def show_Content = cdr match {
    case _: SList => super.show_Content
    case x => Some(s"(${car.show} . ${x.show})")
  }
}

case object SNil extends SList {
  override protected def print_String = "nil"
  def append(p: SList) = p
  def append(p: List[SExpr]) = SList.create(p)
}

case class SLambda(parameters: List[String], expressions: List[SExpr]) extends SExpr {
  // override def getString = Some(toString)
  // def print = toString
  // def show = print
}
object SLambda {
  def create(p: SExpr): SLambda = {
    def raise = RAISE.syntaxErrorFault(s"Illegal lambda: $p")
    try {
      val ps = p.carOrRaise match {
        case m: SCell => m.list.map {
          case SAtom(name) => name
          case _ => raise
        }
        case m => raise
      }
      val body = p.cdrOrRaise match {
        case m: SCell => m.list
        case m => raise
      }
      SLambda(ps, body)
    } catch {
      case NonFatal(e) => raise
    }
  }
}

case class SError(
  label: Option[String],
  exception: Option[Throwable],
  incident: Option[Incident],
  errors: Option[NonEmptyVector[SError]]
) extends SExpr {
  def message: String = label orElse incident.map(_.show) orElse exception.map(_.toString) getOrElse errors.map(_.list.map(_.show).mkString(";")).getOrElse("")
  // override def print = detailTitle
  // override def show = detailTitle
  override def titleInfo = message
  override def descriptionContent = Vector(
    exception.flatMap(e =>
      Option(e.getStackTrace).flatMap(ss =>
        if (ss.isEmpty)
          None
        else
          Some("Stack:" +: ss.map(_.toString).toVector)
      )
    )
  ).flatten.flatten
}
object SError {
  def apply(p: String): SError = SError(Some(p), None, None, None)
  def apply(p: Throwable): SError = SError(None, Some(p), None, None)
  def apply(label: String, e: Throwable): SError = SError(Some(label), Some(e), None, None)
  def apply(req: Request, res: Response): SError = {
    val i = RestIncident(req, res)
    SError(None, None, Some(i), None)
  }
  def apply(req: Request, e: Throwable): SError = {
    val i = RestIncident(req, e)
    SError(None, Some(e), Some(i), None)
  }
  // def apply(p: Response): SError = SError(None, None, None, Some(p), None)
  def apply(i: Incident): SError = SError(None, None, Some(i), None)
  def apply(label: String, i: Incident): SError = SError(Some(label), None, Some(i), None)
  def apply(ps: NonEmptyList[SError]): SError = ps.list match {
    case x :: Nil => x
    case _ => SError(None, None, None, Some(NonEmptyVector(ps.head, ps.tail.toVector)))
  }

  def apply(ps: NonEmptyVector[SError]): SError = {
    if (ps.tail.isEmpty)
      ps.head
    else
      SError(None, None, None, Some(NonEmptyVector(ps.head, ps.tail.toVector)))
  }


  def create(p: SError, ps: Iterable[SError]): SError = apply(NonEmptyVector(p, ps.toVector))

  def invalidArgument(key: String, value: String): SError = {
    val s = s"Invalid parameter '$key': $value"
    SError(Some(s), None, None, None)
  }

  def notFound(label: String, key: String): SError = {
    val s = s"${label}: '$key' is not found."
    SError(Some(s), None, None, None)
  }

  def functionNotFound(p: SAtom): SError = functionNotFound(p.name)

  def functionNotFound(name: String): SError = {
    val label = s"Function '$name' is not found."
    SError(Some(label), None, None, None)
  }

  def bindingNotFound(name: String): SError = {
    val label = s"Binding '$name' is not found."
    SError(Some(label), None, None, None)
  }

  def stackUnderflow: SError = SError(Some("Stack underflow."), None, None, None)

  def invalidDatatype(name: String, p: SExpr): SError = {
    val label = s"Invalid datatype '$name': $p"
    SError(Some(label), None, None, None)
  }

  def Unevaluatable(p: SExpr): SError = {
    val label = s"Unevaluatable expression: $p"
    SError(Some(label), None, None, None)
  }

  def syntaxError(p: String): SError = SError(p)
}

case class SMetaCommand(command: String, args: List[String]) extends SExpr {
  // def print = show
  // def show = toString
}
object SMetaCommand {
  def apply(p: String): SMetaCommand = Strings.totokens(p) match {
    case Nil => SMetaCommand("help", Nil)
    case x :: xs => SMetaCommand(x, xs)
  }
}

case class SConsoleOutput(output: String) extends SExpr {
  override def print = output
  override protected def show_Content = Some(output)
}

// TODO Document or Voucher
trait IDocument extends IRecord
trait IVoucher extends IRecord

trait IProcess {
  def result: Future[SExpr]
}

class FutureProcess(val result: Future[SExpr]) extends IProcess {
}

class PromiseProcess() extends IProcess {
  val promise: Promise[SExpr] = Promise[SExpr]
  val result: Future[SExpr] = promise.future

  def success(p: SExpr): Unit = promise.success(p)
}

trait IWindow {
  def close(): Unit
}
// trait IMatrix
// trait IPath
// trait IScript

case class SBinary(binary: ChunkBag) extends SExpr {
//  override def print = show
//  override def show = detailTitle
  override def titleInfo = binary.size.toString
  override def descriptionContent = Vector.empty
}

case class SI18NString(string: I18NString) extends SExpr {
  override def getString = Some(string.toString)
  // def print = show
  // def show = string.toString
}

case class SI18NTemplate(template: I18NTemplate) extends SExpr {
  override def getString = Some(template.toString)
  // def print = show
  // def show = template.toString
}

case class SRegex(regex: scala.util.matching.Regex) extends SExpr {
  override def equals(rhs: Any): Boolean = rhs match {
    case SRegex(r) => regex.toString == r.toString
    case _ => false
  }
  override def getString = Some(regex.toString)
  // def print = show
  // def show = regex.toString
}

case class SClob(bag: ChunkBag) extends SExpr {
  override def getString = Some(bag.toText)
  override def titleInfo = bag.size.toString
  // def print = bag.toText
  // def show = "Bag()"
  def text = bag.toText
}

case class SBlob(bag: ChunkBag) extends SExpr {
  override def getString = bag.toTextTry.toOption
  override def titleInfo = bag.size.toString
  // def print = bag.toText
  // def show = "Bag()"
}

case class SDocument(document: IDocument) extends SExpr { // Bean
  override def getString = Some(document.toString)
  // def print = show
  // def show = document.toString
}

case class SVoucher(voucher: IVoucher) extends SExpr {
}

case class SSchema(schema: Schema) extends SExpr {
  override protected def print_String = title
  override def titleInfo = s"${schema.columns.length}"
  override def titleDescription = schema.columns.map(_.show).mkString(";")
  override def descriptionContent = schema.columns.map(_.showlong)
}

case class SQuery(query: Query) extends SExpr {
  // override def print = title
  // override def titleInfo = s"${query.columns.length}"
  // override def titleDescription = query.columns.map(_.show).mkString(";")
  // override def descriptionContent = query.columns.map(_.showlong)
}

case class SRecord(record: IRecord) extends SExpr {
  override def getString = Some(record.show)
  override protected def print_String = record.print
  override protected def show_Content = Some(record.show)
}

case class STable(table: ITable) extends SExpr {
  override def getString = Some(table.toString)
  // def print = show
  // def show = table.toString
}

case class SMatrix(matrix: IMatrix[Double]) extends SExpr {
  override def getString = Some(matrix.toString)
  // def print = show
  // def show = matrix.toString
}

case class SUrl(url: java.net.URL) extends SExpr {
  override def getString = Some(url.toString)
  override lazy val asString = url.toString
  override def asUrl = url
  override def asUri = url.toURI
  override def getInputSource = Some(UrlInputSource(url))
  // def print = show
  // def show = url.toString
}

case class SUrn(urn: Urn) extends SExpr {
  override def getString = Some(urn.toString)
  override lazy val asString = urn.toString
  override def asUri = urn.toURI
  // def print = show
  // def show = urn.toString
}

case class SExpression(expression: String) extends SExpr {
  override def getString = Some(expression)
  override def asString = expression
  // def print = path
  // def show = s"XPath($path)"
}

// ScriptEngine, JEXL
case class SScript(language: Option[String], text: String) extends SExpr {
  override def getString = Some(text)
  // def print = show
  // def show = text
}
object SScript {
  def apply(p: String): SScript = SScript(None, p)
}

case class SProcess(process: IProcess) extends SExpr {
//  override def print = detailTitle
//  override def show = detailTitle
  override def descriptionContent = Vector.empty
  // def print = show
  // def show = toString
  def resolveContext: LispContext = RAISE.notImplementedYetDefect
}

case class SWindow(window: IWindow) extends SExpr {
//  override def print = detailTitle
//  override def show = detailTitle
  override def descriptionContent = Vector.empty
  // def print = show
  // def show = toString
}

case class SSingleQuote() extends SExpr {
  // def print = show
  // def show = "SingleQuote()"
}

case class SBean(o: AnyRef) extends SExpr { // JavaBeans
  override def getString = Some(o.toString)
  // def print = show
  // def show = o.toString
}

sealed trait SXml extends SExpr {
  override def equals(o: Any): Boolean = o match {
    case o: SXml => text == o.text
    case _ => false
  }
  def text: String
  def dom: org.w3c.dom.Node
}
object SXml {
  def apply(p: String): SXml = StringSXml(p)
  def apply(p: org.w3c.dom.Node): SXml = DomSXml(p)

  def createOption(p: String): Option[SXml] = Try(apply(DomParser.parse(p))).toOption
}
case class StringSXml(text: String) extends SXml {
  override def getString = Some(text)
  // def print = text
  // def show = s"Xml(${cut_string(text)})"
  lazy val dom = DomParser.parse(text)
}
case class DomSXml(dom: org.w3c.dom.Node) extends SXml {
  override def toString(): String = text
  lazy val text: String = DomUtils.toText(dom)
  override def getString = Some(text)
  // def print = text
  // def show = s"Xml(${cut_string(text)})"
}

case class SHtml(dom: org.w3c.dom.Node) extends SExpr {
  override def equals(o: Any): Boolean = o match {
    case m: SHtml => text == m.text
    case _ => false
  }
  lazy val text: String = DomUtils.toText(dom)
  override def getString = Some(text)
  override def asString = text
  // def print = show
  // def show = html
}
object SHtml {
  def apply(p: String): SHtml = SHtml(DomUtils.parseHtmlLowerCase(p)) // FUTURE : case sensitive

  private val _doctype_regex = "<!DOCTYPE[ ]+html ".r

  def createOption(p: String): Option[SHtml] = {
    Try {
      if (_doctype_regex.findFirstIn(p).isDefined ||
        p.startsWith("<html>") ||
        p.startsWith("<html ")
      )
        Some(apply(p))
      else
        None
    }.toOption.flatten
  }
}

// JXPath
case class SXPath(path: String) extends SExpr {
  override def getString = Some(path)
  override def asString = path
  // def print = path
  // def show = s"XPath($path)"
}

case class SXslt(xslt: String) extends SExpr {
  override def getString = Some(xslt)
  override def asString = xslt
  // def print = xslt
  // def show = s"XSLT(${cut_string(xslt)})"
  lazy val dom: org.w3c.dom.Node = RAISE.notImplementedYetDefect
}

case class SPug(pug: String) extends SExpr {
  override def getString = Some(pug)
  override def asString = pug
  // def print = pug
  // def show = s"Pug(${cut_string(pug)})"
  lazy val dom = RAISE.notImplementedYetDefect
  lazy val nekodom = RAISE.notImplementedYetDefect
}

sealed trait SJson extends SExpr {
  def text: String
  def json: JsValue
  override def getString = Some(text)
  override def asString = text
}
object SJson {
  def apply(text: String): SJson = StringSJson(text)
  def apply(o: JsValue): SJson = JsValueSJson(o)
//  def apply(o: JsObject): SJson = JsObjectSJson(o)

  def createOption(p: String): Option[SJson] = Try(JsValueSJson(Json.parse(p))).toOption
}
case class StringSJson(text: String) extends SJson {
  // def print = SExpr.toPrint(text)
  // def show = text
  lazy val json = Json.parse(text)
}
case class JsValueSJson(json: JsValue) extends SJson {
  lazy val text = json.toString
}
// case class JsObjectSJson(o: JsObject) extends SJson {
//   lazy val text = o.toString
//   def json = o
// }

case class SDateTime(datetime: DateTime) extends SExpr {
  override def asObject = datetime
  override def getString = Some(datetime.toString)
  // def print = show
  // def show = datetime.toString
}

case class SLocalDateTime(datetime: LocalDateTime) extends SExpr {
  override def asObject = datetime
  override def getString = Some(datetime.toString)
  // def print = show
  // def show = datetime.toString
}

case class SLocalDate(date: LocalDate) extends SExpr {
  override def asObject = date
  override def getString = Some(date.toString)
  // def print = show
  // def show = date.toString
}

case class SLocalTime(time: LocalTime) extends SExpr {
  override def asObject = time
  override def getString = Some(time.toString)
  // def print = show
  // def show = time.toString
}

case class SMonthDay(monthday: MonthDay) extends SExpr {
  override def asObject = monthday
  override def getString = Some(monthday.toString) // ISO8601 (e.g. --05-06)
  // def print = show
  // def show = monthday.toString 

  def month: Int = monthday.getMonthOfYear
  def day: Int = monthday.getDayOfMonth
}

case class SInterval(interval: Interval) extends SExpr {
  override def asObject = interval
  override def getString = Some(interval.toString)
  // def print = show
  // def show = interval.toString
}

case class SDuration(duration: Duration) extends SExpr {
  override def asObject = duration
  override def getString = Some(duration.toString)
  // def print = show
  // def show = duration.toString
}

case class SPeriod(period: Period) extends SExpr {
  override def asObject = period
  override def getString = Some(period.toString)
  // def print = show
  // def show = period.toString
}

case class SCurrency(currency: BigDecimal) extends SExpr { // TODO I18N
  override def asObject = currency
  override def getString = Some(currency.toString)
  // def print = show
  // def show = currency.toString
}

case class SPercent(percent: BigDecimal) extends SExpr {
  override def asObject = percent
  override def getString = Some(percent.toString)
  // def print = show
  // def show = percent.toString
}

case class SUnit(unit: String) extends SExpr {
  override def getString = Some(unit)
  // def print = show
  // def show = unit
}

case class S2DSpace(
  serieses: Vector[S2DSpace.Series],
  chart: Option[Chart]
) extends SExpr {
}
object S2DSpace {
  case class Series(
    name: String,
    label: Option[I18NString],
    elements: Vector[Particle]
  ) {
    def getTooltip(no: Int): Option[String] = None // TODO
    def getLabel(no: Int): Option[String] = Some(name) // TODO
    def getUrl(no: Int): Option[String] = None // TODO
  }
  object Series {
    def apply(name: String, elements: Seq[Particle]): Series = Series(name, None, elements.toVector)
  }

  trait Particle {
    def x: Double
    def y: Double
    def label: Option[String]
  }

  case class Point(x: Double, y: Double, label: Option[String]) extends Particle

  def apply(ps: Seq[S2DSpace.Particle], c: Chart): S2DSpace =
    S2DSpace(Vector(Series("XY", ps)), Some(c))
}

case class S3DSpace(
  serieses: Vector[S3DSpace.Series],
  chart: Option[Chart]
) extends SExpr {

}
object S3DSpace {
  case class Series(
    name: String,
    label: Option[I18NString],
    elements: Vector[Particle]
  ) {
    def getTooltip(no: Int): Option[String] = None // TODO
    def getLabel(no: Int): Option[String] = Some(name) // TODO
    def getUrl(no: Int): Option[String] = None // TODO
  }
  object Series {
    def apply(name: String, elements: Seq[Particle]): Series = Series(name, None, elements.toVector)
  }

  trait Particle {
    def x: Double
    def y: Double
    def z: Double
    def label: Option[String]
  }

  case class Point(x: Double, y: Double, z: Double, label: Option[String]) extends Particle

  def apply(ps: Seq[S3DSpace.Particle], c: Chart): S3DSpace =
    S3DSpace(Vector(Series("XYZ", ps)), Some(c))
}

trait SExtension extends SExpr {
}

trait SControl extends SExpr {
  override lazy val resolve = resolveContext.value
  def resolveContext: LispContext
}

case class SMute(expr: SExpr) extends SExpr {
}

case class SFuture(c: LispContext, f: LispFunction) extends SControl {
  override def getString = Some(f.name)
  // def print = show // TODO
  // def show = s"SFuture(${f.name})"
  private lazy val _future = c.futureForEval(f)
  def start(): SFuture = {
    _future
    this
  }
  def resolveContext: LispContext = c.wait(c, _future)
}

case class SLazy(c: LispContext, f: LispFunction) extends SControl {
  override def getString = Some(f.name)
  // def print = show // TODO
  // def show = s"SLazy(${f.name})"
  def resolveContext: LispContext = f(c)
}

case class SLazyFuture(label: String) extends SControl {
  override def getString = Some(label)
  // def print = show // TODO
  // def show = s"SLazyFuture($label)"
  def resolveContext: LispContext = RAISE.notImplementedYetDefect
}

case class SWait(label: String, body: () => LispContext) extends SControl {
  override def getString = Some(label)
  // def print = show // TODO
  // def show = s"SWait($label)"
  def resolveContext: LispContext = body()
}
object SWait {
  def apply(label: String, p: Future[LispContext], timeout: Duration): SWait = {
    new SWait(label, () => Await.result(p, timeout))
  }
}

case class SFutureWait(label: String, c: LispContext, body: () => LispContext) extends SControl {
  override def getString = Some(label)
  // def print = show // TODO
  // def show = s"SFutureWait($label)"
  private lazy val _future = c.future(label, body)
  def start(): SFutureWait = {
    _future
    this
  }
  def resolveContext: LispContext = c.wait(c, _future)
}
object SFutureWait {
  def create(label: String, c: LispContext)(body: => LispContext): SFutureWait =
    SFutureWait(label, c, () => body)
}

trait SPseudo extends SExpr

case object SOpen extends SPseudo { // List Open
  override def getString = Some("#open")
  // def print = show
  // def show = "#open"
}
case object SClose extends SPseudo { // List Close
  override def getString = Some("#close")
  // def print = show
  // def show = "#close"
}
case object SSpace extends SPseudo { // Space
  override def getString = Some("#space")
  // def print = show
  // def show = "#space"
}
case object SDelimiter extends SPseudo { // Special delimiter ','
  override def getString = Some("#delimiter")
  // def print = show
  // def show = "#delimier"
}

object SExpr {
  case class Description(
    title: String,
    content: Vector[String]
  ) {
    def text(nl: String): String = detail.mkString(nl)
    def detail = title +: content

    def toShort = copy(content = toDetail(content))
  }
  object Description {
    def apply(title: String, content: Seq[String]): Description =
      new Description(title, content.toVector)
  }

  sealed trait CreateStrategy extends NamedValueInstance
  object CreateStrategy extends EnumerationClass[CreateStrategy] {
    val elements = Vector(AutoCreate, BinaryCreate)
  }
  case object AutoCreate extends CreateStrategy {
    val name = "auto"
  }
  case object BinaryCreate extends CreateStrategy {
    val name = "binary"
  }

  def createOrNil(p: Option[Any]): SExpr = p.map(create).getOrElse(SNil)

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
    case m: java.math.BigInteger => SNumber(BigDecimal(m))
    case m: BigInt => SNumber(BigDecimal(m))
    case m: java.math.BigDecimal => SNumber(BigDecimal(m))
    case m: BigDecimal => SNumber(m)
    case m: org.w3c.dom.Node => SXml(m)
    // case m: org.w3c.dom.NodeList => m.getLength match {
    //   case 0 => SNil
    //   case 1 => SXml(m.item(0))
    //   case n =>
    //     val xs = for (i <- 0 until n) yield SXml(m.item(i))
    //     SList.create(xs)
    // }
    case m: org.w3c.dom.NodeList =>
      val xs = for (i <- 0 until m.getLength) yield SXml(m.item(i))
      SList.create(xs)
    case JsNull => SNil
    case JsBoolean(v) => SBoolean(v)
    case JsNumber(v) => SNumber(v)
    case JsString(v) => SString(v)
    case m: JsObject => SJson(m)
    case JsArray(vs) => SList.create(vs.map(SExpr.create))
    case m => RAISE.notImplementedYetDefect(s"SExpr#create(${m})") // SObject(m) // TODO
  }

  def create(s: Option[CreateStrategy], p: Any): SExpr =
    s.map(create(_, p)).getOrElse(create(p))

  def create(s: CreateStrategy, p: Any): SExpr = s match {
    case AutoCreate => p match {
      case m: String => createAuto(m)
      case m: SExpr => normalizeAuto(m)
      case m => create(p)
    }
    case BinaryCreate => RAISE.notImplementedYetDefect
  }

  def createAuto(p: String): SExpr = p.headOption.flatMap {
    case '<' => createHtmlOrXmlOption(p)
    case '{' => SJson.createOption(p)
  }.getOrElse(create(p))

  def createHtmlOrXmlOption(p: String): Option[SExpr] = SHtml.createOption(p) orElse SXml.createOption(p)

  def normalizeAuto(p: SExpr): SExpr = p match {
    case m: SString => createAuto(m.string)
    case m: SClob => m.getString.map(createAuto).getOrElse(create(m))
    case m: SBlob => m.getString.map(createAuto).getOrElse(create(m))
    case m => m
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
    "\"" + escapeString(s) + "\""
  }

  def toPrint(s: String): String = {
    val a = escapeNewlines(Strings.cutstring(s, 76))
    StringUtils.dropRightNewlines(a)
  }

  def toDisplay(s: String): String = {
    val a = escapeNewlines(Strings.cutstring(s, 76))
    StringUtils.dropRightNewlines(a)
  }

  def toShow(s: Option[String]): String = s.map(toShow).getOrElse("")

  def toShow(s: String): String = {
    val a = Strings.cutstring(s, 1024)
    StringUtils.dropRightNewlines(a)
  }

  def toDetail(s: Option[String]): Vector[String] = s.map(toDetail).getOrElse(Vector.empty)

  def toDetail(s: String): Vector[String] = {
    val n = 30
    val a = Strings.tolines(s)
    val b = a.take(n)
    if (a.length > n)
      b :+ "..."
    else
      b
  }

  def toDetail(ps: Seq[String]): Vector[String] = ps.take(30).toVector

  def toFull(s: Option[String]): Vector[String] = s.map(toFull).getOrElse(Vector.empty)

  def toFull(s: String): Vector[String] = Strings.tolines(s)

  def escapeNewlines(s: String): String = 
    if (s.contains('\n') | s.contains('\r') | s.contains('\t')) {
      s.replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")
    } else {
      s
    }

  def escapeString(s: String): String = 
    if (s.contains('\n') | s.contains('\r') | s.contains('\t') |
      s.contains('\"') | s.contains('\\')
    ) {
      s.replace("\\", "\\\\").
        replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t").
        replace("\"", "\\\"")
    } else {
      s
    }

  def run(p: => SExpr): SExpr = try(p) catch {
    case NonFatal(e) => SError(e)
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
