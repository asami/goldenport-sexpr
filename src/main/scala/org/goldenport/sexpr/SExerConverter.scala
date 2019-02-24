package org.goldenport.sexpr

import java.net.{URL, URI}
import org.goldenport.RAISE
import org.goldenport.matrix.IMatrix
import org.goldenport.table.ITable

/*
 * @since   Sep. 25, 2018
 *  version Oct. 28, 2018
 * @version Feb. 12, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait SExprConverter[T] extends Function1[SExpr, T]

object SExprConverter {
  implicit case object IntConverter extends SExprConverter[Int] {
    def apply(p: SExpr): Int = p.asInt
  }

  implicit case object toStringConverter extends SExprConverter[String] {
    def apply(p: SExpr): String = p.asString
  }

  implicit case object toUrlConverter extends SExprConverter[URL] {
    def apply(p: SExpr): URL = p.asUrl
  }

  implicit case object toUriConverter extends SExprConverter[URI] {
    def apply(p: SExpr): URI = p.asUri
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
