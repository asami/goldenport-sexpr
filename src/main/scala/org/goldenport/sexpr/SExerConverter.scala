package org.goldenport.sexpr

import java.net.{URL, URI}
import org.goldenport.RAISE
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.{Record, ITable}
import org.goldenport.record.store.Query
import org.goldenport.matrix.IMatrix

/*
 * @since   Sep. 25, 2018
 *  version Oct. 28, 2018
 *  version Feb. 12, 2019
 *  version Mar.  2, 2019
 *  version Apr. 20, 2019
 *  version May.  8, 2019
 *  version Aug.  3, 2019
 * @version Nov.  8, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait SExprConverter[T] extends Function1[SExpr, T]

object SExprConverter {
  implicit case object toSExprConverter extends SExprConverter[SExpr] {
    def apply(p: SExpr): SExpr = p
  }

  implicit case object toSAtomConverter extends SExprConverter[SAtom] {
    def apply(p: SExpr): SAtom = p match {
      case m: SAtom => m
      case m => RAISE.invalidArgumentFault(s"Not atom: $m")
    }
  }

  implicit case object IntConverter extends SExprConverter[Int] {
    def apply(p: SExpr): Int = p.asInt
  }

  implicit case object LongConverter extends SExprConverter[Long] {
    def apply(p: SExpr): Long = p.asLong
  }

  implicit case object FloatConverter extends SExprConverter[Float] {
    def apply(p: SExpr): Float = p.asFloat
  }

  implicit case object DoubleConverter extends SExprConverter[Double] {
    def apply(p: SExpr): Double = p.asDouble
  }

  implicit case object toStringConverter extends SExprConverter[String] {
    def apply(p: SExpr): String = p.asString
  }

  implicit case object toSymbolConverter extends SExprConverter[Symbol] {
    def apply(p: SExpr): Symbol = Symbol(p.asString)
  }

  implicit case object toUrlConverter extends SExprConverter[URL] {
    def apply(p: SExpr): URL = p.asUrl
  }

  implicit case object toUriConverter extends SExprConverter[URI] {
    def apply(p: SExpr): URI = p.asUri
  }

  implicit case object toSchemaConverter extends SExprConverter[Schema] {
    import org.goldenport.sexpr.eval.SchemaFactory
    def apply(p: SExpr): Schema = p match {
      case SSchema(s) => s
      case SString(s) => SchemaFactory.unmarshall(s)
      case m: SJson => SchemaFactory.unmarshall(m.text)
      case m: SXml => SchemaFactory.unmarshall(m.text)
      case m: SHtml => SchemaFactory.unmarshall(m.text)
      case m: SCell => SchemaFactory.unmarshall(m)
      case m => RAISE.invalidArgumentFault(s"Not schema: $m")
    }
  }

  // implicit case object toQueryConverter extends SExprConverter[Query] {
  //   import org.goldenport.sexpr.eval.QueryFactory
  //   def apply(p: SExpr): Query = p match {
  //     case SQuery(s) => s
  //     case SString(s) => QueryFactory.unmarshall(s)
  //     case m: SJson => QueryFactory.unmarshall(m.text)
  //     case m: SXml => QueryFactory.unmarshall(m.text)
  //     case m: SHtml => QueryFactory.unmarshall(m.text)
  //     case m: SExpr => QueryFactory.unmarshall(m)
  //     case m => RAISE.invalidArgumentFault(s"Not query: $m")
  //   }
  // }

  implicit case object toRecordConverter extends SExprConverter[Record] {
    import org.goldenport.sexpr.eval.RecordFactory
    def apply(p: SExpr): Record = p match {
      case SRecord(s) => s.toRecord
      case SString(s) => RecordFactory.unmarshall(s)
      case m: SJson => RecordFactory.unmarshall(m.text)
      case m: SXml => RecordFactory.unmarshall(m.text)
      case m: SHtml => RecordFactory.unmarshall(m.text)
      case m: SCell => RecordFactory.unmarshall(m)
      case m => RAISE.invalidArgumentFault(s"Not record: $m")
    }
  }

  implicit case object toIMatrixConverter extends SExprConverter[IMatrix[Double]] {
    def apply(p: SExpr): IMatrix[Double] = p match {
      case SMatrix(matrix) => matrix
      case m => RAISE.invalidArgumentFault(s"Not matrix: $m")
    }
  }

  implicit case object toITableConverter extends SExprConverter[ITable] {
    def apply(p: SExpr): ITable = p match {
      case STable(table) => table
      case SMatrix(matrix) => ???
      case m => RAISE.invalidArgumentFault(s"Not table: $m")
    }
  }
}
