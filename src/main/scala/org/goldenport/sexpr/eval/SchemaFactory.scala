package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import org.w3c.dom.{Document, Element}
import play.api.libs.json.{JsValue, Json}
import org.goldenport.exception._
import org.goldenport.hocon.RichConfig
import org.goldenport.record.v2.{Schema, SchemaFactory => RSchemaFactory, ImporterFactory, _}
import org.goldenport.xml.dom.DomParser
import org.goldenport.sexpr._

/*
 * @since   Apr. 14, 2019
 * @version Apr. 20, 2019
 * @author  ASAMI, Tomoharu
 */
case class SchemaFactory(config: RichConfig) {
  private val _schema_factory = {
    val importerfactory = ImporterFactory(Vector.empty)
    RSchemaFactory(importerfactory)
  }

  def unmarshall(p: String): Schema = unmarshallValidation(p).valueOr(e => throw e.head)

  def unmarshallValidation(p: String): ValidationNel[Throwable, Schema] = try {
    if (p.startsWith("{"))
      unmarshallValidation(Json.parse(p))
    else if (p.startsWith("("))
      unmarshallValidation(SExprParserNew.apply(p))
    else if (p.startsWith("<"))
      unmarshallValidation(DomParser.parse(p))
    else
      Failure(SyntaxErrorFaultException(s"Illegal schema: $p")).toValidationNel

  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshall(p: SExpr): Schema = SchemaFactory.SExprSchemaParser.parse(p)

  def unmarshallValidation(p: JsValue): ValidationNel[Throwable, Schema] = try {
    Success(unmarshall(p)).toValidationNel
  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshall(p: JsValue): Schema = _schema_factory.unmarshall(p)

  def unmarshallValidation(p: SExpr): ValidationNel[Throwable, Schema] = try {
    Success(SchemaFactory.SExprSchemaParser.parse(p))
  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshallValidation(p: Document): ValidationNel[Throwable, Schema] =
    Failure(SyntaxErrorFaultException(s"Illegal schema: $p")).toValidationNel
}

object SchemaFactory {
  val default = SchemaFactory(RichConfig.empty)

  def unmarshall(p: String): Schema = default.unmarshall(p)
  def unmarshall(p: SExpr): Schema = default.unmarshall(p)
  def unmarshall(p: SJson): Schema = default.unmarshall(p)
  def unmarshall(p: SXml): Schema = default.unmarshall(p)

  object SExprSchemaParser extends SExprParsers {
    import org.goldenport.record.v2.{Column, DataType, Multiplicity}
    import org.goldenport.record.sql.SqlDatatype

    def parse(s: String): Schema = parse(SExprParserNew(s))

    def parse(expr: SExpr): Schema = {
      val reader = SExprReader.create(expr)
      rule(reader) match {
        case Success(result, _) => result
        case failure: NoSuccess => sys.error(failure.msg) // TODO
      }
    }

    def rule: Parser[Schema] = {
      open ~> expr.* <~ close ^^ {
        case exprs => ???
      }
    }
  }
}
