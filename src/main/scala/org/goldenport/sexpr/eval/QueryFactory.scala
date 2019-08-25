package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import org.w3c.dom.{Document, Element}
import play.api.libs.json.{JsValue, Json}
import org.goldenport.exception._
import org.goldenport.hocon.RichConfig
import org.goldenport.record.v2.{Schema, SchemaFactory => RSchemaFactory, ImporterFactory, _}
import org.goldenport.record.store.Query
import org.goldenport.record.query._
import org.goldenport.xml.dom.DomParser
import org.goldenport.sexpr._

/*
 * @since   Apr. 20, 2019
 *  version Apr. 20, 2019
 * @version Aug.  2, 2019
 * @author  ASAMI, Tomoharu
 */
case class QueryFactory(config: RichConfig) {
  def unmarshall(p: String): Query = unmarshallValidation(p).valueOr(e => throw e.head)

  def unmarshallValidation(p: String): ValidationNel[Throwable, Query] = try {
    if (p.startsWith("{"))
      unmarshallValidation(Json.parse(p))
    else if (p.startsWith("("))
      unmarshallValidation(SExprParserNew.apply(p))
    else if (p.startsWith("<"))
      unmarshallValidation(DomParser.parse(p))
    else
      Failure(SyntaxErrorFaultException(s"Illegal query: $p")).toValidationNel

  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshall(p: SExpr): Query = QueryFactory.SExprQueryParser.parse(p)

  def unmarshallValidation(p: JsValue): ValidationNel[Throwable, Query] = try {
    Success(unmarshall(p)).toValidationNel
  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshall(p: JsValue): Query = RAISE.notImplementedYetDefect

  def unmarshallValidation(p: SExpr): ValidationNel[Throwable, Query] = try {
    Success(QueryFactory.SExprQueryParser.parse(p))
  } catch {
    case NonFatal(e) => Failure(e).toValidationNel
  }

  def unmarshallValidation(p: Document): ValidationNel[Throwable, Query] =
    Failure(SyntaxErrorFaultException(s"Illegal query: $p")).toValidationNel
}

object QueryFactory {
  val default = QueryFactory(RichConfig.empty)

  def unmarshall(p: String): Query = default.unmarshall(p)
  def unmarshall(p: SExpr): Query = default.unmarshall(p)
  def unmarshall(p: SJson): Query = default.unmarshall(p)
  def unmarshall(p: SXml): Query = default.unmarshall(p)

  object SExprQueryParser extends SExprParsers {
    import org.goldenport.record.v2.{Column, DataType, Multiplicity}
    import org.goldenport.record.sql.SqlDatatype

    def parse(s: String): Query = parse(SExprParserNew(s))

    def parse(expr: SExpr): Query = {
      val reader = SExprReader.create(expr)
      rule(reader) match {
        case Success(result, _) => result
        case failure: NoSuccess => sys.error(failure.msg) // TODO
      }
    }

    def rule: Parser[Query] = {
      open ~> query_expr.* <~ close ^^ {
        case exprs => Query(exprs)
      }
    }

    def query_expr: Parser[Query.Expression] = and | or | not | equal | not_equal

    def and: Parser[Query.AndExpression] = {
      open ~ atom("and") ~> query_expr.* <~ close ^^ {
        case exprs => Query.AndExpression(exprs)
      }
    }

    def or: Parser[Query.OrExpression] = {
      open ~ atom("or") ~> query_expr.* <~ close ^^ {
        case exprs => Query.OrExpression(exprs)
      }
    }

    def not: Parser[Query.NotExpression] = {
      open ~ atom("not") ~> query_expr.* <~ close ^^ {
        case exprs => Query.NotExpression(exprs)
      }
    }

    def equal: Parser[Query.Expression] = {
      open ~ atom("=") ~> atom_name ~ expr <~ close ^^ {
        case atom ~ expr => Query.QueryExpressionExpression(
          atom,
          EqualQuery(expr.asObject)
        )
      }
    }

    def not_equal: Parser[Query.Expression] = {
      open ~ atom("!=") ~> atom_name ~ expr <~ close ^^ {
        case atom ~ expr => Query.QueryExpressionExpression(
          atom,
          NotEqualQuery(expr.asObject)
        )
      }
    }
  }
}
