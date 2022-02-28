package org.goldenport.sexpr

import scalaz.NonEmptyList
import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration.{Duration => ScalaDuration}
import java.util.Locale
import java.net.{URL, URI}
import java.io.File
import java.lang.reflect.InvocationTargetException
import org.w3c.dom.Node
import org.joda.time._
import play.api.libs.json._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.parser._
import org.goldenport.context.{StatusCode, Conclusion, Consequence}
import org.goldenport.i18n.{I18NString, I18NTemplate}
import org.goldenport.i18n.I18NMessage
import org.goldenport.console.Message
import org.goldenport.collection.NonEmptyVector
import org.goldenport.extension.IWindow
import org.goldenport.extension.{IDocument => GDocument}
import org.goldenport.extension.Showable
import org.goldenport.matrix.{IMatrix, Matrix}
import org.goldenport.record.v2.{Schema, Column, XDouble}
import org.goldenport.record.v3.{IRecord, Record, ITable, Table, RecordSequence}
import org.goldenport.record.http.{Request, Response}
import org.goldenport.record.store.Query
import org.goldenport.record.chart.{Chart, Space, Series, Particle}
import org.goldenport.value._
import org.goldenport.values.{Urn, NumberRange, ValueRange, NumberInterval, DateTimePeriod, Money, Percent}
import org.goldenport.values.LocalDateTimeInterval
import org.goldenport.io.{InputSource, StringInputSource, UrlInputSource, UriUtils}
import org.goldenport.io.MimeType
import org.goldenport.bag.{ChunkBag, StringBag}
import org.goldenport.bag.{ClobBag, BlobBag}
import org.goldenport.xml.XmlConstants._
import org.goldenport.xml.dom.{DomParser, DomUtils}
import org.goldenport.parser.{LogicalToken, ParseResult}
import org.goldenport.parser.ScriptToken
import org.goldenport.parser.CommandParser
import org.goldenport.xsv.Lxsv
import org.goldenport.util.{StringUtils, AnyRefUtils, AnyUtils}
import org.goldenport.util.DateTimeUtils
import org.goldenport.util.SpireUtils
import org.goldenport.sexpr.eval.{LispContext, LispFunction, Incident, RestIncident}
import org.goldenport.sexpr.eval.{InvariantIncident, PreConditionIncident, PreConditionStateIncident, PostConditionIncident}
import org.goldenport.sexpr.eval.entity.Entity
import org.goldenport.sexpr.eval.spark.SparkDataFrame
import org.goldenport.sexpr.script.Script
// import org.goldenport.sexpr.eval.chart.Chart

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
 *  version May. 21, 2019
 *  version Jun. 30, 2019
 *  version Jul. 29, 2019
 *  version Aug. 25, 2019
 *  version Sep. 29, 2019
 *  version Oct. 31, 2019
 *  version Nov. 30, 2019
 *  version Dec. 30, 2019
 *  version Jan. 30, 2020
 *  version Feb. 29, 2020
 *  version Mar.  1, 2020
 *  version Jan. 31, 2021
 *  version Feb. 25, 2021
 *  version Mar. 18, 2021
 *  version Apr. 25, 2021
 *  version May. 30, 2021
 *  version Jun. 27, 2021
 *  version Jul. 12, 2021
 *  version Sep. 25, 2021
 *  version Oct. 31, 2021
 *  version Nov. 21, 2021
 *  version Feb. 25, 2022
 * @version Mar.  1, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait SExpr extends Showable {
  override def toString(): String = display
  def isNilOrFalse: Boolean = false
  def getList: Option[List[SExpr]] = None
  //  def asNumber: SNumber = RAISE.unsupportedOperationFault(s"Not number(${this})")
  def getString: Option[String] = None
  def asObject: Any = getString getOrElse this
  def asJavaObject: Object = asObject.asInstanceOf[Object] // used by java features (e.g. formatter)
  def asRecordOrObject: Any = asObject
  def asRecordOrJavaObject: Any = asJavaObject
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

  /* Natural representation for data. Show as-is even large data. */
  /* SString("apple") => apple */
  def print: String = print_value
  lazy protected val print_value: String = print_make
  protected def print_make: String = try {
    SExpr.toPrint(print_String)
  } catch {
    case NonFatal(e) => s"${getClass.getSimpleName}#print[$e]"
  }
  protected def print_String: String = getString getOrElse title

  /* Literal representation in Programming Language. */
  /* SString("apple") => "apple" */
  final def literal: String = literal_String

  protected def literal_String: String = print

  /* Exchange format over media like file or network. */
  /* SString("apple") => apple */
  final def marshall: String = marshall_String

  protected def marshall_String: String = literal

  /* 1 line representation for interaction representation (e.g. REPL). */
  /* SString("apple") => "apple" */
  def display: String = display_value
  protected lazy val display_value = display_make
  protected def display_make = try {
    SExpr.toDisplay(display_String)
  } catch {
    case NonFatal(e) => s"${getClass.getSimpleName}#display[$e]"
  }
  protected def display_String: String = literal
  private lazy val _title_name = {
    val s = getClass.getSimpleName
    if (s(0) == 'S')
      s.tail
    else
      s
  }
  def titleName: String = _title_name
  def titleInfo: String = ""
  def titleDescription: String = ""
  def title: String = title_value
  lazy protected val title_value = title_make
  protected def title_make = titleInfo match {
    case "" => s"${titleName}"
    case i => s"${titleName}[$i]"
  }
  def longTitle: String = long_title_value
  lazy protected val long_title_value: String = long_title
  protected def long_title: String = {
    (titleInfo, titleDescription) match {
      case ("", "") => s"${titleName}"
      case ("", d) => s"${titleName}:$d"
      case (i, "") => s"${titleName}($i)"
      case (i, d) => s"${titleName}($i):$d"
    }
  }

  /* Show a shortened natural representation with some information for debug. */
  /* SString("apple") => SString[5]: apple */
  def show: String = show_value
  lazy protected val show_value: String = show_make
  protected def show_make: String = try {
    val title = s"[$longTitle]"
    show_Content.fold(title) { s =>
      val a = SExpr.toShow(s)
      if (titleDescription.nonEmpty)
        s"${title}\n${a}"
      else if (a.contains('\n') || a.contains('\r'))
        s"${title}\n${a}"
      else
        s"${title}${a}"
    }
  } catch {
    case NonFatal(e) => s"${getClass.getSimpleName}#show[$e]"
  }
  protected def show_Content: Option[String] = getString // for debug

  protected lazy val description_value = description_make
  protected def description_make = full_description.toShort

  /*
   * Show natural description of the data.
   */
  def description: SExpr.Description = description_value
  def descriptionContent: Seq[String] = SExpr.toFull(getString)
  protected lazy val full_description_value = full_description
  protected def full_description = {
    val c = descriptionContent
    if (descriptionContent.isEmpty)
      SExpr.Description(longTitle, c)
    else
      SExpr.Description(title, c)
  }

  /*
   * Show full description of the data.
   */
  def fullDescription: SExpr.Description = full_description

  /*
   * Show short representation to embeded in container format like table.
   */
  def embed: String = display

  /*
   * Beautified representation.
   */
  def pretty: Seq[String] = SExpr.toFull(print)

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

  def toBag: Either[BlobBag, ClobBag] = Right(ClobBag.create(marshall))
  def toChunkBag: ChunkBag = toBag match {
    case Right(r) => r
    case Left(l) => l
  }

  def carOrRaise: SExpr = RAISE.syntaxErrorFault(s"$this")
  def cdrOrRaise: SExpr = RAISE.syntaxErrorFault(s"$this")

  protected final def cut_string(p: String) = Strings.cutstring(p, 32)
}

trait IDom { self: SExpr =>
  def dom: org.w3c.dom.Node
}

// trait IWindow {
//   def close(): Unit
// }

// class JavaFXWindow(window: org.goldenport.javafx.JavaFXWindow) extends IWindow {
//   def close() = window.close()
// }

// trait IMatrix
// trait IPath
// trait IScript
// TODO Document or Voucher
trait IDocument extends GDocument
trait IVoucher extends IRecord // obsolated
trait ISlip extends IRecord

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

trait IDataFrame {
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
  override def getString = Some(s":$name")
  // def print = show
  // def show = ":" + name
}

case class SNumber(number: spire.math.Number) extends SExpr {
  override def asObject = number
  override def asJavaObject = SpireUtils.toJavaNumber(number)
  override lazy val asString = number.toString
  override def getString = Some(asString)
  // def print = show
  // def show = number.toString
  override lazy val asInt: Int = number.toInt
  override lazy val asLong: Long = number.toLong
  override lazy val asFloat: Float = number.toFloat
  override lazy val asDouble: Double = number.toDouble
  override def asBigDecimal = number.toBigDecimal
  def toRange: SRange = SRange(ValueRange(number))

  def +(rhs: SNumber): SNumber = SNumber(number + rhs.number)
  def -(rhs: SNumber): SNumber = SNumber(number - rhs.number)
  def *(rhs: SNumber): SNumber = SNumber(number * rhs.number)
  def *(rhs: SMatrix): SMatrix = rhs * this
  def /(rhs: SNumber): SNumber = SNumber(number / rhs.number)
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
object SRational {
  def apply(numerator: Long, denominator: Long): SRational = SRational(spire.math.Rational(numerator, denominator))
}

case class SComplex(number: spire.math.Complex[Double]) extends SExpr {
  override lazy val asString = number.toString
  override def getString = Some(asString)
  // def print = show
  // def show = number.toString
}
object SComplex {
  def apply(real: Double, img: Double): SComplex = SComplex(spire.math.Complex(real, img))
}

case class SBoolean(value: Boolean) extends SExpr {
  override def asObject: Any = value
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
      case m: SError => false
      case _ => true
    }
  }
}

case class SRange(range: NumberRange) extends SExpr {
  override protected def print_String = range.print
}

case class SInterval(interval: NumberInterval) extends SExpr {
  override protected def print_String = interval.print

  def toRange: SRange = SRange(interval.toRange)
}

case class SString(string: String) extends SExpr {
  override def toString() = s"SString($string)"
  override def getString = Some(string)
  override def asString = string
  override protected def print_String = string
  override protected def literal_String = SExpr.toStringLiteral(string)
  override protected def marshall_String = string
  override protected def display_String = literal_String
  override def titleInfo = s"${string.length}"

  def replace(from: String, to: String): SString = SString(string.replace(from, to))
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

case class SLambda(
  labelOption: Option[String],
  parameters: List[String],
  expressions: List[SExpr]
) extends SExpr {
  // override def getString = Some(toString)
  // def print = toString
  // def show = print

  def label: String = labelOption.map(x => s"Lambda:$x").getOrElse("Lambda")
}
object SLambda {
  def apply(
    parameters: List[String],
    expressions: List[SExpr]
  ): SLambda = SLambda(None, parameters, expressions)

  def apply(
    label: String,
    parameters: List[String],
    expressions: List[SExpr]
  ): SLambda = SLambda(Some(label), parameters, expressions)

  def create(label: String, p: SExpr): SLambda = {
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
      SLambda(Some(label), ps, body)
    } catch {
      case NonFatal(e) => raise
    }
  }

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
      SLambda(None, ps, body)
    } catch {
      case NonFatal(e) => raise
    }
  }
}

case class SError(
  conclusion: Conclusion,
  // labelOption: Option[String] = None,
  // exception: Option[Throwable] = None,
  incident: Option[Incident] = None,
  errors: Option[NonEmptyVector[SError]] = None,
  stdout: Option[SBlob] = None,
  stderr: Option[SBlob] = None
) extends SExpr {
  def exception = conclusion.exception
  // def label = Some(message)

  def RAISE: Nothing = throw new SError.SErrorException(this)

  override def asObject = exception getOrElse this
  def message: String = _incident_message orElse _exception_message orElse _error_message getOrElse conclusion.message
  private def _incident_message = incident.map(_.show)
  private def _exception_message = exception.map(_.toString)
  private def _error_message = errors.map(_.list.map(_.show).mkString(";"))

  // override def print = detailTitle
  // override def show = detailTitle
  override def titleInfo = message
  protected override def show_Content = {
    exception.flatMap(e =>
      Option(e.getStackTrace).flatMap(ss =>
        if (ss.isEmpty)
          None
        else
          Some("===Stack==\n" + ss.map(_.toString).mkString("\n"))
      )
    ) orElse stderr.flatMap(_.getString.flatMap(s =>
      if (s.isEmpty)
        None
      else
        Some("===Stderr=== \n" + s)
    ))
  }
  override def descriptionContent = Vector(
    exception.flatMap(e =>
      Option(e.getStackTrace).flatMap(ss =>
        if (ss.isEmpty)
          None
        else
          Some("===Stack===" +: ss.map(_.toString).toVector)
      )
    )
  ).flatten.flatten
}
object SError {
  private def _bad_request = Conclusion.BadRequest
  private def _unauthorized_request = Conclusion.Unauthorized
  private def _not_found = Conclusion.NotFound
  private def _internal_server_error = Conclusion.InternalServerError
  private def _not_implemented = Conclusion.NotImplemented
  private def _no_reach = Conclusion.NoReach
  private def _invariant = Conclusion.Invariant
  private def _pre_condition = Conclusion.PreCondition
  private def _pre_condition_state = Conclusion.PreConditionState
  private def _post_condition = Conclusion.PostCondition

  def apply(conclusion: Conclusion, msg: String): SError = SError(conclusion.withMessage(msg))
  def apply(conclusion: Conclusion, incident: Incident): SError = SError(conclusion, Some(incident))
  def apply(p: String): SError = SError(_internal_server_error)
  def apply(p: Throwable): SError = SError(Conclusion.make(p)) // , None, Some(_normalize(p)), None, None)
  def apply(label: String, e: Throwable): SError = SError(Conclusion.make(e, label)) // , Some(label), Some(_normalize(e)), None, None)
  def apply(start: Long, req: Request, res: Response): SError = {
    val i = RestIncident(start, req, res)
    SError(res.toConclusion, Some(i))
  }
  def apply(start: Long, req: Request, e: Throwable): SError = {
    val i = RestIncident(start, req, _normalize(e))
    SError(Conclusion.make(e), Some(i))
  }
  // def apply(p: Response): SError = SError(None, None, None, Some(p), None)
  def apply(i: Incident): SError = SError(_internal_server_error, Some(i))
  def apply(label: String, i: Incident): SError = SError(_internal_server_error.withMessage(label), Some(i))
  def apply(ps: NonEmptyList[SError]): SError = ps.list match {
    case x :: Nil => x
    case _ => SError(ps.head.conclusion, None, Some(NonEmptyVector(ps.head, ps.tail.toVector)))
  }
  def apply(p: String, stdout: SBlob, stderr: SBlob): SError =
    SError(_internal_server_error.withMessage(p), None, None, Some(stdout), Some(stderr))

  def apply(ps: NonEmptyVector[SError]): SError = {
    if (ps.tail.isEmpty)
      ps.head
    else
      SError(_internal_server_error, None, Some(NonEmptyVector(ps.head, ps.tail.toVector)))
  }

  def apply(c: Conclusion, s: String, params: Seq[SExpr]): SError =
    apply(c, I18NMessage(s, params.map(_.embed)))

  def apply(c: Conclusion, s: I18NMessage): SError = apply(c.withMessage(s))

  def create(p: SError, ps: Iterable[SError]): SError = apply(NonEmptyVector(p, ps.toVector))

  def merge(label: String, e: SErrorException): SError = SError(e.error.conclusion.withMessage(label),  errors = Some(NonEmptyVector(e.error)))

  def invalidArgument(key: String, value: String): SError = {
    val s = s"Invalid parameter '$key': $value"
    SError(_bad_request, s)
  }

  def notFound(label: String, key: String): SError = {
    val s = s"${label}: '$key' is not found."
    SError(_not_found, s)
  }

  def notFound(key: Symbol): SError = notFound(key.name)

  def notFound(key: String): SError = {
    val s = s"'${key}' is not found."
    SError(_not_found, s)
  }

  def notFoundIn(key: SExpr, target: SExpr): SError = {
    val s = "{0} is not found in [{1}]."
    SError(_not_found, s, List(key, target))
  }

  def functionNotFound(p: SAtom): SError = functionNotFound(p.name)

  def functionNotFound(name: String): SError = {
    val label = s"Function '$name' is not found."
    SError(_not_found, label)
  }

  def bindingNotFound(name: String): SError = {
    val label = s"Binding '$name' is not found."
    SError(_not_found, label)
  }

  def stackUnderflow: SError = SError(_internal_server_error, "Stack underflow.")

  def invalidArgumentFault(p: String): SError = SError(_bad_request, p)

  def missingArgumentFault(p: String): SError = SError(_bad_request, p) // TODO

  def invalidDatatype(name: String, p: SExpr): SError =
    invalidDatatype(name, p.embed)

  def invalidDatatype(name: String, p: String): SError = {
    val label = s"Invalid datatype '$name': $p"
    SError(_bad_request, label)
  }

  def unevaluatable(p: SExpr): SError = {
    val label = s"Unevaluatable expression: $p"
    SError(_bad_request, label)
  }

  def unevaluatableParameter(p: SExpr): SError = {
    val label = s"Unevaluatable parameter: $p"
    SError(_bad_request, label)
  }

  def syntaxError(p: String): SError = SError(_bad_request, p)

  def syntaxError(p: ParseFailure[_]): SError = {
    val label = p.message
    SError(_bad_request, label)
  }

  def invalidConversion(p: String): SError = SError(_bad_request, p)

  def invariant(): SError = SError(_invariant)
  def invariant(p: SExpr): SError = SError(_invariant, InvariantIncident(p))
  def invariant(p: SMessage): SError = SError(_invariant, InvariantIncident(p))

  def preCondition(): SError = SError(_pre_condition)
  def preCondition(p: SExpr): SError = SError(_pre_condition, PreConditionIncident(p))
  def preCondition(p: SMessage): SError = SError(_pre_condition, PreConditionIncident(p))

  def preConditionState(): SError = SError(_pre_condition_state)
  def preConditionState(p: SExpr): SError = SError(_pre_condition_state, PreConditionStateIncident(p))
  def preConditionState(p: SMessage): SError = SError(_pre_condition_state, PreConditionStateIncident(p))

  def postCondition(): SError = SError(_post_condition)
  def postCondition(p: SExpr): SError = SError(_post_condition, PostConditionIncident(p))
  def postCondition(p: SMessage): SError = SError(_post_condition, PostConditionIncident(p))

  def notImplementedYetDefect(p: String): SError = SError(_not_implemented, p)

  def noReachDefect(p: String): SError = SError(_no_reach, p)

  private def _normalize(p: Throwable): Throwable = p match {
    case m: InvocationTargetException => Option(m.getTargetException) orElse Option(m.getCause) getOrElse m
    case m => m
  }

  case class SErrorException(val error: SError) extends RuntimeException(error.message) {
  }
}

case class SException() extends SExpr {
}

case class SLongJump() extends SExpr {
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

case class SConsoleOutput(output: Message) extends SExpr {
  override protected def print_String = output.text
  override def display = output.text
  override protected def show_Content = Some(output.text)
}

case class SConsequence(value: SExpr, conclusion: Conclusion) extends SExpr {
  override protected def print_String = s"""Consequence(${value.embed})"""
  override def display = s"""Consequence(${value.embed})"""
  override protected def show_Content = Some(display)
}

case class SBinary(
  binary: ChunkBag
) extends SExpr {
//  override def print = show
//  override def show = detailTitle
  override def titleInfo = binary.size.toString
  override def descriptionContent = Vector.empty
  def mimetype: MimeType = binary.mimetype
}
object SBinary {
  def create(p: ChunkBag): SBinary = create(MimeType.application_octet_stream, "bin", p)

  def create(mime: MimeType, suffix: String, p: ChunkBag): SBinary = {
    val bag = BlobBag.create("binary", suffix, mime, p)
    SBinary(bag)
  }
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

case class SMessage(
  i18nMessage: I18NMessage
) extends SExpr {
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
object SRegex {
  import scala.util.matching.Regex

  def create(p: String): SExpr = SExpr.createOrError(SRegex(new Regex(p)))

  def create(p: String, groupnames: Seq[String]): SExpr = SExpr.createOrError(SRegex(new Regex(p, groupnames: _*)))
}

case class SClob(bag: ClobBag) extends SExpr {
  override def pretty = SExpr.toFullPrettyGuess(bag.toString)
  override def getString = Some(bag.toText)
  override def titleInfo = bag.size.toString
  // def print = bag.toText
  // def show = "Bag()"
  def text = bag.toText
}
object SClob {
  def apply(p: String): SClob = SClob(ClobBag.create(p))
  def apply(p: ChunkBag): SClob = SClob(ClobBag.create(p))
}

case class SBlob(bag: BlobBag) extends SExpr {
  override def pretty = bag.toTextTry.toOption.map(SExpr.toFullPrettyGuess).getOrElse(super.pretty)
  override def getString = bag.toTextTry.toOption
  override def titleInfo = bag.size.toString
  // def print = bag.toText
  // def show = "Bag()"
}
object SBlob {
  def apply(p: ChunkBag): SBlob = SBlob(BlobBag.create(p))
  def text(p: String): SBlob = SBlob(BlobBag.create(p))
}

// literal document format (e.g. smartdox)
case class SDocument(document: IDocument) extends SExpr {
  override def getString = Some(document.toString)
  // def print = show
  // def show = document.toString
}

case class SSlip(slip: ISlip) extends SExpr {
}

// obsolated
case class SVoucher(voucher: IVoucher) extends SExpr {
}

case class SEntity(entity: Entity) extends SExpr with Mutable {
  override def asObject: Any = entity
  override def pretty = table.pretty
  override def embed = record.embed
  override def getString = Some(record.show)
  override protected def print_String = record.print
  override def titleInfo = entity.show
  override protected def show_Content = Some(record.show)

  def attributes: IRecord = entity.attributes
  def record: IRecord = entity.record
  def table: STable = STable(Table.create(record))


  def id: SExpr = SExpr.create(entity.id.string)
}

case class SSchema(schema: Schema) extends SExpr {
  override protected def print_String = title
  override def titleInfo = s"${schema.columns.length}"
//  override def titleDescription = schema.columns.map(_.show).mkString(";")
  override def descriptionContent = schema.columns.map(_.showlong)

  override protected def display_String = s"""$title ${schema.columns.map(_.show).mkString("\t")}"""
  override protected def show_Content: Option[String] = Some(schema.show)
}

case class SQuery(query: Query) extends SExpr {
  // override def print = title
  // override def titleInfo = s"${query.columns.length}"
  // override def titleDescription = query.columns.map(_.show).mkString(";")
  // override def descriptionContent = query.columns.map(_.showlong)
}
object SQuery {
  def create(p: String): SQuery = {
    val q = RAISE.notImplementedYetDefect
    SQuery(q)
  }
}

case class SRecord(record: IRecord) extends SExpr {
  override def asObject: Any = record
  override def pretty = table.pretty
  override def embed = record.embed
  override def getString = Some(record.show)
  override protected def print_String = record.print
  override def titleInfo = s"${record.length}"
  override protected def show_Content = Some(record.show)

  def get(propertyname: String): SExpr = SExpr.create(record.get(propertyname))

  def table: STable = STable(Table.create(record))
  def xml: SXml = SXml(record)
  def json: SJson = SJson(record.toRecord.toJson)
}
object SRecord {
  def create(p: Lxsv): SRecord = SRecord(Record.create(p))

  def parseLxsv(p: String): ParseResult[SRecord] = Lxsv.parseToken(p).map(create)
}

case class STable(table: ITable) extends SExpr {
  override def asObject: Any = table
  override def pretty = SExpr.toFull(table.show)
  override def getString = Some(display)
  override val titleInfo = s"${table.width}x${table.height}"
  override protected def print_String = table.print
  override protected def display_String = table.display
  override protected def show_Content: Option[String] = Some(table.show)

  def matrix: SMatrix = SMatrix(table.toTable.matrixDoubleDistilled)

  // def matrix: SMatrix = {
  //   val schema = table.schema
  //   val xs: Vector[Vector[Double]] = _vector.map(_matrix_row(_matrix_columns, _))
  //   val mx = Matrix.createDouble(xs)
  //   SMatrix(mx)
  // }

//  def dataframe: SDataFrame = SDataFrame(SparkDataFrame.create(table))

  def width = table.width
  def height = table.height
  def isEmpty = table.height == 0
  lazy val schema: SExpr = SSchema(table.schema)
  def head: SExpr = _table.headOption.map(SRecord(_)).getOrElse(SNil)
  def tail: STable = STable(_table.tail)
  def ensureHead: STable = STable(_table.ensureHead)
  def list: SList = SList.create(_vector)
  def vector: SVector = SVector(_vector)
  def row(y: Int): SExpr = _table.getRow(y).map(SRecord(_)).getOrElse(SNil)
  def column(x: Int): SVector = SVector.lift(_table.column(x))
  def column(x: String): SVector = SVector.lift(_table.column(x))
  def at(x: Int, y: Int): SExpr = SExpr.create(_table.get(x, y))
  def at(x: String, y: Int): SExpr = SExpr.create(_table.get(x, y))

  private lazy val _table = table.toTable
  private lazy val _vector = table.toRecordVector.map(SRecord(_))

  def toRecordSequence: RecordSequence = RecordSequence(table.toRecordVector)
}
object STable {
  def data(ps: Seq[Record]): STable = RAISE.notImplementedYetDefect
}

case class SVector(vector: Vector[SExpr]) extends SExpr {
  override def asObject: Any = vector
  override val titleInfo = s"${vector.length}"
  override protected def print_String = vector.map(_.print).mkString("[", " ", "]")
  override protected def display_String = vector.map(_.embed).mkString("[", " ", "]")
  override protected def show_Content: Option[String] = Some(display)
  def length = vector.length
  def list: SList = SList.create(vector)
  def head: SExpr = vector.headOption.getOrElse(SNil)
  def tail: SVector = SVector(vector.tail)
  def at(i: Int): SExpr = vector(i)
  def +(rhs: SVector): SVector = RAISE.notImplementedYetDefect
  def -(rhs: SVector): SVector = RAISE.notImplementedYetDefect
}
object SVector {
  def create(ps: Seq[SExpr]): SVector = SVector(ps.toVector)
  def lift(ps: Seq[Any]): SVector = SVector(ps.toVector.map(SExpr.create))
}

case class SMatrix(matrix: IMatrix[Double]) extends SExpr {
  override def asObject: Any = matrix
  override def getString = Some(display)
  override val titleInfo = s"${matrix.width}x${matrix.height}"
  override protected def print_String = matrix.print
  override protected def display_String = matrix.display
  override protected def show_Content: Option[String] = Some(matrix.show)

  def row(i: Int): SVector = SVector(matrix.rowVector(i).map(SNumber(_)))
  def column(i: Int): SVector = SVector(matrix.columnVector(i).map(SNumber(_)))
  def at(x: Int, y: Int): SNumber = SNumber(matrix.apply(x, y))

  def table: STable = STable(Table.createDouble(matrix))

//  def dataframe: SDataFrame = SDataFrame.create(matrix)

  def transpose: SMatrix = SMatrix(matrix.transpose)
  def t: SMatrix = transpose
  def inv: SMatrix = SMatrix(matrix.inv)
  def det: SNumber = SNumber(matrix.det)
  def rank: SNumber = SNumber(matrix.rank)
  def +(rhs: SMatrix): SMatrix = SMatrix(matrix + rhs.matrix)
  def -(rhs: SMatrix): SMatrix = SMatrix(matrix - rhs.matrix)
  def *(rhs: SMatrix): SMatrix = SMatrix(matrix * rhs.matrix)
  def *(rhs: SVector): SMatrix = SMatrix(_multify(rhs.vector))
  def *(rhs: SList): SMatrix = SMatrix(_multify(rhs.vector))
  def *(rhs: SNumber): SMatrix = SMatrix(matrix * rhs.asDouble)

  private def _multify(ps: Vector[SExpr]) = {
    if (matrix.width != ps.length)
      RAISE.invalidArgumentFault(s"Vector length(${ps.length}) is not equals matrix width(${matrix.width}).")
    val xs = ps.map(_.asDouble)
    matrix * xs
  }
}
object SMatrix {
  import org.goldenport.matrix._

  val CMD_TRANSPORSE = "transpose"
  val CMD_HORIZONTAL = "horizontal"
  val CMD_VERTICAL = "vertical"

  private val _parser_1d = CommandParser.create(CMD_TRANSPORSE, CMD_HORIZONTAL, CMD_VERTICAL)
  private val _parser_2d = CommandParser.create(CMD_TRANSPORSE)

  def create(prefix: Option[String], p: String): SMatrix = create2d(prefix, p)

  def create2d(prefix: Option[String], p: String): SMatrix =
    prefix.map(_matrix_2d(_, p)).getOrElse(_matrix_2d(p))

  def create1d(prefix: Option[String], p: String): SMatrix = _matrix_1d(prefix, p)

  private def _matrix_1d(prefix: Option[String], p: String): SMatrix =
    prefix.map(_matrix_1d(_, p)).getOrElse(_matrix_1d_horizontal(p))

  private def _matrix_1d(prefix: String, p: String): SMatrix =
    _parser_1d.get(prefix).collect {
      case CMD_TRANSPORSE => _matrix_1d_vertical(p)
      case CMD_VERTICAL => _matrix_1d_vertical(p)
      case CMD_HORIZONTAL => _matrix_1d_horizontal(p)
    }.getOrElse(_matrix_1d_horizontal(p))

  private def _matrix_1d_vertical(p: String) = {
    val a = Strings.totokens(p).map(_.toDouble)
    val b = VectorColumnRowMatrix.create(Vector(a))
    SMatrix(b)
  }

  private def _matrix_1d_horizontal(p: String) = {
    val a = Strings.totokens(p).map(_.toDouble)
    val b = VectorRowColumnMatrix.create(Vector(a))
    SMatrix(b)
  }

  private def _matrix_2d(prefix: String, p: String): SMatrix =
    _parser_2d.get(prefix).collect {
      case CMD_TRANSPORSE => _matrix_2d(p).transpose
    }.getOrElse(RAISE.invalidArgumentFault(s"Unknown prefix: $prefix"))

  private def _matrix_2d(p: String): SMatrix = {
    val a = p.dropWhile(_ == '[')
    val b = a.takeWhile(_ != ']')
    val c = Strings.tolines(b).filter(Strings.notblankp)
    val d = c.map(x => Strings.totokens(x).map(_.toDouble))
    val e = VectorRowColumnMatrix.create(d)
    SMatrix(e)
  }

  def data(ps: Seq[Seq[Double]]): SMatrix = RAISE.notImplementedYetDefect
}

case class SDataFrame(dataframe: IDataFrame) extends SExpr {
  def table: STable = RAISE.notImplementedYetDefect
  def matrix: SMatrix = RAISE.notImplementedYetDefect
}
object SDataFrame {
  // import org.goldenport.sexpr.eval.spark.SparkDataFrame

  // def create(p: IMatrix[Double]): SDataFrame = SDataFrame(SparkDataFrame.create(p))
}

case class SLxsv(lxsv: Lxsv) extends SExpr {
  override def equals(o: Any): Boolean = o match {
    case m: SLxsv => lxsv == m.lxsv
    case _ => false
  }
  lazy val text: String = lxsv.print
  override def getString = Some(text)
  override def asString = text
  override protected def print_String = text
  override def titleInfo = s"${lxsv.length}"
}
object SLxsv {
  val suffix = "lxsv"

  def apply(p: String): SLxsv = SLxsv(Lxsv.create(p))

  def createOption(p: String): Option[SLxsv] = Lxsv.createOption(p).map(SLxsv.apply)
}

case class SUrl(url: java.net.URL) extends SExpr {
  override def getString = Some(url.toString)
  override lazy val asString = url.toString
  override def asUrl = url
  override def asUri = url.toURI
  override def getInputSource = Some(UrlInputSource(url))
  // def print = show
  // def show = url.toString

  def asSUri: SUri = SUri(asUri)

  def getSuffix: Option[String] = Option(url.getFile).flatMap(StringUtils.getSuffix)

  def isXmlFamily = isXml || isHtml || isXsl
  def isXml = getSuffix.map(_ == SXml.suffix) getOrElse false
  def isHtml = getSuffix.map(_ == SHtml.suffix) getOrElse false
  def isXsl = getSuffix.map(_ == SXsl.suffix) getOrElse false
}
object SUrl {
  def apply(p: String): SUrl = SUrl(new URL(p))

  def makeOption(p: String): Option[SUrl] = 
    if (UrlToken.isUrl(p))
      Try(SUrl(p)).toOption
    else
      None
}

case class SUrn(urn: Urn) extends SExpr {
  override lazy val getString = Some(asString)
  override lazy val asString = urn.text
  override def asUri = urn.toURI
  def asSUri: SUri = SUri(asUri)
  // def print = show
  // def show = urn.toString
}
object SUrn {
  def apply(p: String): SUrn = SUrn(Urn(p))

  def makeOption(p: String): Option[SUrn] =
    if (UrnToken.isUrn(p))
      Try(SUrn(p)).toOption
    else
      None
}

case class SUri(uri: URI) extends SExpr {
  override lazy val getString = Some(asString)
  override lazy val asString = uri.toString
  override def asUri = uri
  // def print = show
  // def show = urn.toString

  def getFile: Option[File] = UriUtils.getFile(uri)
  def getUrl: Option[URL] = UriUtils.getUrl(uri)
  def getUrn: Option[Urn] = UriUtils.getUrn(uri)
}
object SUri {
  def apply(p: String): SUri = SUri(new URI(p))

  def makeOption(p: String): Option[SUri] =
    if (UriToken.isUri(p))
      Try(SUri(p)).toOption
    else
      None

  def makeUrxOption(p: String): Option[SExpr] =
    SUrl.makeOption(p) orElse SUrn.makeOption(p) orElse makeOption(p)
}

case class SExpression(expression: String) extends SExpr {
  override def getString = Some(expression)
  override def asString = expression
  // def print = path
  // def show = s"XPath($path)"
}

// ScriptEngine, JEXL
case class SScript(
  language: Option[String],
  properties: Record,
  text: String,
  format: Option[String]
) extends SExpr {
  override def getString = Some(text)
  // def print = show
  // def show = text
}
object SScript {
  def apply(p: String): SScript = SScript(None, Record.empty, p, None)
  def apply(prefix: String, p: String): SScript = SScript(Some(prefix), Record.empty, p, None)
  def apply(prefix: Option[String], p: String): SScript = SScript(prefix, Record.empty, p, None)
  def apply(prefix: Option[String], properties: Option[String], p: String, postfix: Option[String]): SScript =
    SScript(prefix, Record.fromLxsv(properties), p, postfix)

  def create(p: ScriptToken): SScript = SScript(p.prefix, p.properties, p.text, p.postfix)
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

case class SBean(o: AnyRef) extends SExpr { // JavaBeans or Java Object
  override def getString = Some(o.toString)
  // def print = show
  // def show = o.toString
}

sealed trait SXml extends SExpr with IDom {
  override def equals(o: Any): Boolean = o match {
    case o: SXml => text == o.text
    case _ => false
  }
  def text: String
  def dom: org.w3c.dom.Node
  override def pretty = SExpr.toFull(SExpr.toPrettyXml(dom))

  //
  def json: SExpr = Record.createRecordOrSequence(dom) match {
    case Right(r) => SJson(r.toJson)
    case Left(l) => SJson(l.toJson)
  }
  def record: SExpr = Record.createRecordOrSequence(dom) match {
    case Right(r) => SRecord(r)
    case Left(l) => SError.invalidConversion("record")
  }
  def table: SExpr = Record.createRecordOrSequence(dom) match {
    case Right(r) => STable(Table.create(r))
    case Left(l) => STable(l.toTable)
  }
  def recordOrTable: SExpr = Record.createRecordOrSequence(dom) match {
    case Right(r) => SRecord(r)
    case Left(l) => STable(l.toTable)
  }
}
object SXml {
  val suffix = "xml"

  def apply(p: String): SXml = StringSXml(p)
  def apply(p: org.w3c.dom.Node): SXml = DomSXml(p)

  def createOption(p: String): Option[SXml] = Try(apply(DomParser.parse(p))).toOption

  def create(config: Script.Config, p: String): SExpr = createOption(p).
    map(_to_sexpr).
    getOrElse(SError.syntaxError(Strings.cutstring(p)))

  private def _to_sexpr(p: SXml) = {
    val dom = p.dom
    dom.getNamespaceURI() match {
      case null => _to_sexpr_prefix(p)
      case NS_XHTML => SHtml(p.dom)
      case NS_XSL => SXsl(p.dom)
      case _ => _to_sexpr_prefix(p)
    }
  }

  private def _to_sexpr_prefix(p: SXml) = {
    val dom = p.dom
    dom.getPrefix() match {
      case "xsl" => SXsl(dom)
      case "xhtml" => SHtml(dom)
      case _ => p
    }
  }
}
case class StringSXml(text: String) extends SXml {
  override val titleName = "XML(String)"
  override def getString = Some(text)
  override protected def print_String = text
  // def show = s"Xml(${cut_string(text)})"
  lazy val dom = DomParser.parse(text)
}
case class DomSXml(dom: org.w3c.dom.Node) extends SXml {
  override val titleName = "XML(DOM)"
  override def toString(): String = text
  lazy val text: String = DomUtils.toText(dom)
  override def getString = Some(text)
  override protected def print_String = text
  // def show = s"Xml(${cut_string(text)})"
}
case class NodeListSXml(nodelist: org.w3c.dom.NodeList) extends SXml {
  override val titleName = "XML(NodeList)"
  override def toString(): String = text
  lazy val dom = RAISE.notImplementedYetDefect
  lazy val text: String = DomUtils.distillText(nodelist)
  override def getString = Some(text)
  override protected def print_String = text
  // def show = s"Xml(${cut_string(text)})"
}

case class SHtml(dom: org.w3c.dom.Node) extends SExpr with IDom {
  override def equals(o: Any): Boolean = o match {
    case m: SHtml => text == m.text
    case _ => false
  }
  lazy val text: String = DomUtils.toText(dom)
  override def pretty = SExpr.toFull(SExpr.toPrettyHtml(dom))
  override def getString = Some(text)
  override def asString = text
  override protected def print_String = text
  // def show = html
}
object SHtml {
  val suffix = "html"

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

sealed trait SXsl extends SExpr with IDom {
}
case class StringSXsl(xslt: String) extends SXsl {
  override def getString = Some(xslt)
  override def asString = xslt
  // def print = xslt
  // def show = s"XSLT(${cut_string(xslt)})"
  lazy val dom: org.w3c.dom.Node = DomUtils.parseXml(xslt)
}
case class DomSXsl(dom: org.w3c.dom.Node) extends SXsl {
}
object SXsl {
  val suffix = "xsl"
  private val _doctype_regex = "<!DOCTYPE[ ]+html ".r

  def apply(p: String): SXsl = StringSXsl(p)
  def apply(p: org.w3c.dom.Node): SXsl = DomSXsl(p)

  def createOption(p: String): Option[SXsl] = {
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

case class SPug(pug: String) extends SExpr {
  override def getString = Some(pug)
  override def asString = pug
  // def print = pug
  // def show = s"Pug(${cut_string(pug)})"
  lazy val dom = RAISE.notImplementedYetDefect
  lazy val nekodom = RAISE.notImplementedYetDefect
}

sealed trait SJson extends SExpr with IDom {
  def text: String
  def json: JsValue
  override def pretty = SExpr.toFull(Json.prettyPrint(json))
  override def getString = Some(text)
  override def asString = text

  lazy val dom = Record.createRecordOrSequence(json) match {
    case Right(r) => r
    case Left(l) => l
  }

  //
  def xml: SExpr = SXml(dom)
  def record: SExpr = Record.createRecordOrSequence(json) match {
    case Right(r) => SRecord(r)
    case Left(l) => SError.invalidConversion("record")
  }
  def table: SExpr = Record.createRecordOrSequence(json) match {
    case Right(r) => STable(Table.create(r))
    case Left(l) => STable(l.toTable)
  }
  def recordOrTable: SExpr = Record.createRecordOrSequence(json) match {
    case Right(r) => SRecord(r)
    case Left(l) => STable(l.toTable)
  }
}
object SJson {
  def apply(text: String): SJson = StringSJson(text)
  def apply(o: JsValue): SJson = JsValueSJson(o)
//  def apply(o: JsObject): SJson = JsObjectSJson(o)

  def createOption(p: String): Option[SJson] = Try(JsValueSJson(Json.parse(p))).toOption
}
case class StringSJson(text: String) extends SJson {
  override val titleName = "JSON(String)"
  // def print = SExpr.toPrint(text)
  // def show = text
  lazy val json = Json.parse(text)
}
case class JsValueSJson(json: JsValue) extends SJson {
  override val titleName = "JSON(JsValue)"
  lazy val text = json.toString
}
// case class JsObjectSJson(o: JsObject) extends SJson {
//   lazy val text = o.toString
//   def json = o
// }

sealed trait SHocon extends SExpr with IDom {
  def text: String
  def hocon: Hocon
  override def pretty = SExpr.toFull(text)
  override def getString = Some(text)
  override def asString = text
  lazy val record: Record = Record.createConfig(hocon)
  def dom = record
}
object SHocon {
  def apply(text: String): SHocon = StringSHocon(text)
  def apply(o: Hocon): SHocon = HoconSHocon(o)
  //  def apply(o: JsObject): SHocon = JsObjectSHocon(o)

  def createOption(p: String): Option[SHocon] = Try(ConfigFactory.parseString(p)).toOption.map(SHocon.apply)
}
case class StringSHocon(text: String) extends SHocon {
  def hocon = RAISE.notImplementedYetDefect
  override val titleName = "HOCON(String)"
  // def print = SExpr.toPrint(text)
  // def show = text
}
case class HoconSHocon(hocon: Hocon) extends SHocon {
  def text = RAISE.notImplementedYetDefect
  override val titleName = "HOCON(HOCON)"
}

case class SDateTime(datetime: DateTime) extends SExpr {
  override def asObject = datetime
//  override def asJavaObject = new java.time.ZonedDateTime() // Java 8
  override def asJavaObject = datetime.toCalendar(Locale.US)
  override def getString = Some(datetime.toString)
  // def print = show
  // def show = datetime.toString
}
object SDateTime {
  import DateTimeUtils.jodajst

  def jst(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): SDateTime =
    SDateTime(new DateTime(year, month, day, hour, minute, second, jodajst))
}

case class SLocalDateTime(datetime: LocalDateTime) extends SExpr {
  override def asObject = datetime
//  override def asJavaObject = new java.time.LocalDateTime() // Java 8
  override def asJavaObject = new java.sql.Timestamp(datetime.toDateTime.getMillis)
  override def getString = Some(datetime.toString)
  // def print = show
  // def show = datetime.toString
}
object SLocalDateTime {
  def apply(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): SLocalDateTime =
    SLocalDateTime(new LocalDateTime(year, month, day, hour, minute, second))
}

case class SLocalDate(date: LocalDate) extends SExpr {
  override def asObject = date
  override def getString = Some(date.toString)
  // def print = show
  // def show = date.toString
}
object SLocalDate {
  def apply(y: Int, m: Int, d: Int): SLocalDate = SLocalDate(new LocalDate(y, m, d))
}

case class SLocalTime(time: LocalTime) extends SExpr {
  override def asObject = time
  override def getString = Some(time.toString)
  // def print = show
  // def show = time.toString
}
object SLocalTime {
  def apply(h: Int, m: Int, s: Int): SLocalTime = SLocalTime(new LocalTime(h, m, s))
}

case class SMonthDay(monthday: MonthDay) extends SExpr {
  override def asObject = monthday
  override def getString = Some(monthday.toString) // ISO8601 (e.g. --05-06)
  // def print = show
  // def show = monthday.toString 

  def month: Int = monthday.getMonthOfYear
  def day: Int = monthday.getDayOfMonth
}
object SMonthDay {
  def apply(m: Int, d: Int): SMonthDay = SMonthDay(new MonthDay(m, d))
}

/*
 * An interval in Joda-Time represents an interval of time from one millisecond instant to another instant.
 * 
 * See org.joda.time.Interval
 * See https://stackoverflow.com/questions/2653567/joda-time-whats-the-difference-between-period-interval-and-duration
 */
case class SDateTimeInterval(interval: DateTimePeriod) extends SExpr {
  override def asObject = interval
  override def getString = Some(interval.toString)
  // def print = show
  // def show = interval.toString
}
object SDateTimeInterval {
  def create(p: String): SDateTimeInterval = SDateTimeInterval(DateTimePeriod.parse(p))
}

case class SLocalDateTimeInterval(interval: LocalDateTimeInterval) extends SExpr {
  override def asObject = interval
  override def getString = Some(interval.toString)
  // def print = show
  // def show = interval.toString
}
object SLocalDateTimeInterval {
  def create(p: String): SLocalDateTimeInterval = SLocalDateTimeInterval(LocalDateTimeInterval.create(p))
}

/*
 * A duration in Joda-Time represents a duration of time measured in milliseconds.
 * 
 * See scala.concurrent.duration.Duration
 * See https://stackoverflow.com/questions/2653567/joda-time-whats-the-difference-between-period-interval-and-duration
 */
case class SDuration(duration: Duration, label: Option[String] = None) extends SExpr {
  override def asObject = duration
  override def getString = label orElse Some(duration.toString)
  // def print = show
  // def show = duration.toString
}
object SDuration {
  import org.goldenport.util.DurationUtils

  def apply(p: ScalaDuration): SDuration = SDuration(new Duration(p.toMillis))

  def parse(p: String): ParseResult[SDuration] = DurationUtils.parseJoda(p).map(SDuration(_, Some(p)))

  def create(p: String): SDuration = parse(p).take

  def hour(h: Int): SDuration = SDuration(DurationUtils.hour(h))
}

/*
 * A period in Joda-Time represents a period of time defined in terms of fields, for example, 3 years 5 months 2 days and 7 hours.
 * 
 * See org.joda.time.Period
 * See https://stackoverflow.com/questions/2653567/joda-time-whats-the-difference-between-period-interval-and-duration
 */
case class SPeriod(period: Period) extends SExpr {
  override def asObject = period
  override def getString = Some(period.toString)
  // def print = show
  // def show = period.toString
}
object SPeriod {
  import org.goldenport.util.PeriodUtils

  def parse(p: String): ParseResult[SPeriod] = PeriodUtils.parse(p).map(SPeriod.apply)

  def create(p: String): SPeriod = parse(p).take

  def yearMonthDay(y: Int, m: Int, d: Int): SPeriod = SPeriod(PeriodUtils.yearMonthDay(y, m, d))
}

case class SImage(binary: BlobBag) extends SExpr {
  override def asObject = binary
  override def toBag = Left(binary)
  def mimetype: MimeType = binary.mimetype
}
object SImage {
  def png(p: ChunkBag): SImage = create("png", MimeType.image_png, p)
  def webp(p: ChunkBag): SImage = create("webp", MimeType.image_webp, p)
  def svg(p: ChunkBag): SImage = create("svg", MimeType.image_svg_xml, p)

  def create(suffix: String, mime: MimeType, p: ChunkBag): SImage = {
    val bag = BlobBag.create("image", suffix, mime, p)
    SImage(bag)
  }
}

case class SMoney(money: Money) extends SExpr {
  override def asObject = money
  override def getString = Some(money.toString)
  // def print = show
  // def show = currency.toString
}
object SMoney {
  def doller(p: Int): SMoney = RAISE.notImplementedYetDefect
  def yen(p: Int): SMoney = RAISE.notImplementedYetDefect
}

case class SPercent(percent: Percent) extends SExpr {
  override def asObject = percent
  override def getString = Some(percent.toString)
  // def print = show
  // def show = percent.toString
}
object SPercent {
  def apply(p: Double): SPercent = RAISE.notImplementedYetDefect
}

case class SUnit(unit: String) extends SExpr {
  override def getString = Some(unit)
  // def print = show
  // def show = unit
}

case class SChart(
  chart: Chart
) extends SExpr {
}

object SChart {
}

case class SChartSpace(
  space: Space
) extends SExpr {
}
object SChartSpace {
  def apply(name: String, xlabel: String, ylabel: String, ps: Seq[Particle]): SChartSpace = {
    val s = Series.shape(name, xlabel, ylabel, ps)
    SChartSpace(Space(Vector(s)))
  }
}

case class SChartSeries(
  series: Series
) extends SExpr {
}

object SChartSeries {
  def apply(name: String, ps: Seq[Particle]): SChartSeries =
    SChartSeries(Series.shape(name, ps))

  def apply(name: String, xlabel: String, ylabel: String, ps: Seq[Particle]): SChartSeries =
    SChartSeries(Series.shape(name, xlabel, ylabel, ps))
}

// case class SSpace2D(
//   space: Space2D
// ) extends SExpr {
// }

// object SSpace2D {
//   def apply(name: String, ps: Seq[Particle], c: Chart): SSpace2D = {
//     val s = Series.shape(name, ps)
//     SSpace2D(Space2D(Vector(s), Some(c)))
//   }
// }

// case class S2DSpace(
//   serieses: Vector[S2DSpace.Series],
//   chart: Option[Chart]
// ) extends SExpr {
// }
// object S2DSpace {
//   case class Series(
//     name: String,
//     label: Option[I18NString],
//     elements: Vector[Particle],
//     linesVisible: Boolean,
//     shapsVisible: Boolean
//   ) {
//     def getLabel(no: Int): Option[String] = elements.lift(no).flatMap(_.label)
//     def getTooltip(no: Int): Option[String] = elements.lift(no).flatMap(_.tooltip)
//     def getUrl(no: Int): Option[URL] = elements.lift(no).flatMap(_.url)
//   }
//   object Series {
//     def shape(name: String, elements: Seq[Particle]): Series = Series(name, None, elements.toVector, false, true)
//     def shape(name: String, label: Option[I18NString], elements: Seq[Particle]): Series = new Series(name, label, elements.toVector, false, true)
//     def line(ps: Seq[Point]): Series = new Series("#line", None, ps.toVector, true, false)
//   }

//   trait Particle {
//     def x: Double
//     def y: Double
//     def label: Option[String]
//     def tooltip: Option[String]
//     def url: Option[URL]
//   }

//   case class Point(
//     x: Double,
//     y: Double,
//     label: Option[String],
//     tooltip: Option[String],
//     url: Option[URL]
//   ) extends Particle
//   object Point {
//     def apply(x: Double, y: Double, label: String): Point =
//       Point(x, y, Some(label), None, None)

//     def apply(x: Double, y: Double): Point =
//       Point(x, y, None, None, None)
//   }

//   def apply(name: String, ps: Seq[S2DSpace.Particle], c: Chart): S2DSpace = {
//     val s = Series.shape(name, ps)
//     S2DSpace(Vector(s), Some(c))
//   }
// }

// case class S3DSpace(
//   serieses: Vector[S3DSpace.Series],
//   chart: Option[Chart]
// ) extends SExpr {

// }
// object S3DSpace {
//   case class Series(
//     name: String,
//     label: Option[I18NString],
//     elements: Vector[Particle]
//   ) {
//     def getLabel(no: Int): Option[String] = elements.lift(no).flatMap(_.label)
//     def getTooltip(no: Int): Option[String] = elements.lift(no).flatMap(_.tooltip)
//     def getUrl(no: Int): Option[URL] = elements.lift(no).flatMap(_.url)
//   }
//   object Series {
//     def apply(name: String, elements: Seq[Particle]): Series = Series(name, None, elements.toVector)
//   }

//   trait Particle {
//     def x: Double
//     def y: Double
//     def z: Double
//     def label: Option[String]
//     def tooltip: Option[String]
//     def url: Option[URL]
//   }

//   case class Point(
//     x: Double,
//     y: Double,
//     z: Double,
//     label: Option[String],
//     tooltip: Option[String],
//     url: Option[URL]
//   ) extends Particle

//   def apply(ps: Seq[S3DSpace.Particle], c: Chart): S3DSpace =
//     S3DSpace(Vector(Series("XYZ", ps)), Some(c))
// }

trait SExtension extends SExpr {
}

trait Mutable { self: SExpr =>
  override def print = print_make
  override def display = display_make
  override def title = title_make
  override def longTitle = long_title
  override def show = show_make
  override def description = description_make
}

trait SControl extends SExpr with Mutable {
  override lazy val resolve = resolveContext.value
  def resolveContext: LispContext
}

/*
 * Control object to indicate invisible in a stack.
 */
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
object SFuture {
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
  def apply(label: String, p: Future[LispContext], timeout: ScalaDuration): SWait = {
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
    case null => SNil // result of ScriptEngine
    case m: SExpr => m
    case m: Array[_] => _create_seq(m)
    case m: Seq[_] => _create_seq(m)
    case m: Boolean => SBoolean.create(m)
    case m: Byte => SNumber(m)
    case m: Short => SNumber(m)
    case m: Int => SNumber(m)
    case m: Long => SNumber(m)
    case m: Float => SNumber(m)
    case m: Double => SNumber(m)
    case m: NumberRange => SRange(m)
    case m: String => SString(m)
    case m: java.math.BigInteger => SNumber(BigDecimal(m))
    case m: BigInt => SNumber(BigDecimal(m))
    case m: java.math.BigDecimal => SNumber(BigDecimal(m))
    case m: BigDecimal => SNumber(m)
    case m: IRecord => SRecord(m)
    case m: ITable => STable(m)
    case m: IMatrix[_] => SMatrix(m.asInstanceOf[IMatrix[Double]]) // XXX
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
    case m: Hocon => SHocon(m)
    case m: LogicalToken => SExprParserNew.parse(m)
    case m: Throwable => SError(m)
    case m: AnyRef => SBean(m)
    case m => SBean(AnyRefUtils.toAnyRef(m))
  }

  private def _create_seq(p: Seq[Any]): SExpr =
    if (p.isEmpty)
      SNil
    else if (p.forall(_.isInstanceOf[JsValue]))
      SJson(JsArray(p.map(_.asInstanceOf[JsValue]).toVector))
    else if (p.length > 100)
      SVector.create(p.map(create))
    else
      SList.create(p.map(create))

  def create(s: Option[CreateStrategy], p: Any): SExpr =
    s.map(create(_, p)).getOrElse(create(p))

  def create(s: CreateStrategy, p: Any): SExpr = s match {
    case AutoCreate => p match {
      case m: String => createAuto(m)
      case m: SUrl => createAuto(m)
      case m: SUrn => createAuto(m)
      case m: SUri => createAuto(m)
      case m: SExpr => normalizeAuto(m)
      case m => create(p)
    }
    case BinaryCreate => RAISE.notImplementedYetDefect
  }

  def createAuto(p: String): SExpr = p.headOption.collect {
    case '<' => createXmlFamilyOption(p)
    case '{' => SJson.createOption(p)
  }.flatten.getOrElse(create(p))

  def createAuto(p: SUrl): SExpr = create(p) // Should be resolved in LispContext.
  def createAuto(p: SUrn): SExpr = create(p) // Should be resolved in LispContext.
  def createAuto(p: SUri): SExpr = create(p) // Should be resolved in LispContext.

  def createXmlFamilyOption(p: String): Option[SExpr] =
    SHtml.createOption(p) orElse SXsl.createOption(p) orElse SXml.createOption(p)

  def normalizeAuto(p: SExpr): SExpr = p match {
    case m: SString => createAuto(m.string)
    case m: SClob => m.getString.map(createAuto).getOrElse(create(m))
    case m: SBlob => m.getString.map(createAuto).getOrElse(create(m))
    case m: SUrl => createAuto(m)
    case m: SUrn => createAuto(m)
    case m: SUri => createAuto(m)
    case m => m
  }

  def run[T <: SExpr](p: => Consequence[T]): SExpr = try {
    p match {
      case Consequence.Success(r, c) => r
      case Consequence.Error(c) => SError(c)
    }
  } catch {
    case NonFatal(e) => SError(e)
  }

  def runOrNotFound[T <: SExpr](key: String)(p: => Consequence[Option[T]]): SExpr = try {
    p match {
      case Consequence.Success(r, c) => r getOrElse SError.notFound(key)
      case Consequence.Error(c) => SError(c)
    }
  } catch {
    case NonFatal(e) => SError(e)
  }

  def execute[T <: SExpr](p: => T): SExpr = try {
    p
  } catch {
    case NonFatal(e) => SError(e)
  }

  def executeOrNotFound(key: String)(p: => Option[SExpr]): SExpr =
    p.map(x => execute(x)).getOrElse(SError.notFound(key))

  def orNotFound(key: String, p: Option[SExpr]): SExpr =
    p.getOrElse(SError.notFound(key))

  def createOrError[T <: SExpr](p: => T): SExpr = execute(p)

  def parseOrError[T <: SExpr](p: => ParseResult[T]): SExpr = try {
    p match {
      case ParseSuccess(ast, ws) => ast
      case EmptyParseResult() => SError("Empty")
      case m: ParseFailure[_] => SError(m.message)
    }
  } catch {
    case NonFatal(e) => SError(e)
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

  def toPrint(s: String): String = s

  def toDisplay(s: String): String = {
    val a = escapeDisplay(Strings.cutstring(s, 480))
    StringUtils.dropRightNewlines(a)
  }

  def toShow(s: Option[String]): String = s.map(toShow).getOrElse("")

  def toShow(s: String): String = toShowLong(s)

  def toShowShort(s: String): String = {
    val a = StringUtils.showConsole(s, "\n")
    StringUtils.dropRightNewlines(a)
  }

  def toShowLong(s: String): String = {
    val a = StringUtils.showConsole(s, "\n", 10)
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

  def toFullPrettyGuess(s: String): Vector[String] = toFull(toPrettyGuess(s))

  def toPrettyGuess(s: String): String = Try(
    s.headOption.map {
      case '<' => toPrettyXml(s)
      case '{' => toPrettyJson(s)
      case _ => s
    }.getOrElse(s)
  ).getOrElse(s)

  def toPrettyXml(p: String): String = {
    val dom = DomUtils.parseHtmlFragment(p)
    toPrettyXml(dom)
  }

  def toPrettyXml(dom: Node): String = DomUtils.toPrettyFragmentText(dom)

  def toPrettyHtml(p: String): String = {
    val dom = DomUtils.parseHtmlFragment(p)
    toPrettyHtml(dom)
  }

  def toPrettyHtml(dom: Node): String = DomUtils.toHtmlPrettyText(dom)

  def toPrettyJson(p: String): String = {
    val json = Json.parse(p)
    toPrettyJson(json)
  }

  def toPrettyJson(json: JsValue): String = Json.prettyPrint(json)

  val symboledCRLF = "♼"
  val symboledCR = "♻"
  val symboledLF = "♲"

  def escapeDisplay(s: String): String =
    if (s.contains('\n') | s.contains('\r')) {
//      s.replace("\n", "[NL]").replace("\r", "[CR]")
      s.replace("\r\n", symboledCRLF).replace("\n", symboledLF).replace("\r", symboledCR)
    } else {
      s
    }

  private def escapeNewlines(s: String): String =
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
