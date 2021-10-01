package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import org.w3c.dom.{Document, Element}
import play.api.libs.json.{JsValue, Json}
import org.goldenport.exception._
import org.goldenport.hocon.RichConfig
import org.goldenport.record.v3.{Record, Field, FieldValue, EmptyValue, SingleValue, MultipleValue}
import org.goldenport.record.v2.{Schema, SchemaFactory => RSchemaFactory, ImporterFactory, _}
import org.goldenport.xml.dom.DomParser
import org.goldenport.sexpr._

/*
 * @since   Apr. 20, 2019
 *  version May.  9, 2019
 *  version Jul. 30, 2019
 * @version Sep. 21, 2021
 * @author  ASAMI, Tomoharu
 */
case class RecordFactory(config: RichConfig) {
  def unmarshall(p: String): Record = unmarshallValidation(p).valueOr(e => throw e.head)

  def unmarshallValidation(p: String): ValidationNel[Throwable, Record] = try {
    if (p.startsWith("{"))
      unmarshallValidation(Json.parse(p))
    else if (p.startsWith("("))
      unmarshallValidation(SExprParserNew.apply(p))
    else if (p.startsWith("<"))
      unmarshallValidation(DomParser.parse(p))
    else
      Failure(SyntaxErrorFaultException(s"Illegal record: $p")).toValidationNel

  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshall(p: SExpr): Record = p match {
    case SNil => Record.empty
    case _ => RecordFactory.SExprRecordParser.parse(p)
  }

  def unmarshall(p: SJson): Record = unmarshall(p.json)

  def unmarshallValidation(p: JsValue): ValidationNel[Throwable, Record] = try {
    Success(unmarshall(p)).toValidationNel
  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshall(p: JsValue): Record = Record.create(p)

  def unmarshallValidation(p: SExpr): ValidationNel[Throwable, Record] = try {
    Success(RecordFactory.SExprRecordParser.parse(p))
  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshallValidation(p: Document): ValidationNel[Throwable, Record] =
    Failure(SyntaxErrorFaultException(s"Illegal record: $p")).toValidationNel
}

object RecordFactory {
  val default = RecordFactory(RichConfig.empty)

  def unmarshall(p: String): Record = default.unmarshall(p)
  def unmarshall(p: SList): Record = default.unmarshall(p)
  def unmarshall(p: SJson): Record = default.unmarshall(p)
  def unmarshall(p: SXml): Record = default.unmarshall(p)
  def unmarshall(p: SExpr): Record = default.unmarshall(p)

  object SExprRecordParser extends SExprParsers {
    import org.goldenport.record.v2.{Column, DataType, Multiplicity}
    import org.goldenport.record.sql.SqlDatatype

    def parse(s: String): Record = parse(SExprParserNew(s))

    def parse(expr: SExpr): Record = {
      val reader = SExprReader.create(expr)
      rule(reader) match {
        case Success(result, _) => result
        case Failure(msg, _) => _syntax_error(msg)
        case Error(msg, _) => _syntax_error(msg)
      }
    }

    private def _syntax_error(msg: String) = RAISE.syntaxErrorFault(s"SExprRecordParser: $msg")

    def rule: Parser[Record] = {
      open ~> field.* <~ close ^^ {
        case fields => Record(fields)
      }
    }

    def field: Parser[Field] = keyword_field | cell_field

    def keyword_field: Parser[Field] = {
      keyword_name_expr ^^ {
        case (name, expr) => Field(name, _value(expr))
      }
    }

    def cell_field: Parser[Field] = {
      cell_key_value ^^ {
        case (key, expr) => Field(key, _value(expr))
      }
    }

    private def _value(p: SExpr): FieldValue = p match {
      case SNil => EmptyValue
      case m: SList => MultipleValue(m.list)
      case _ => SingleValue(p)
    }
  }
}
