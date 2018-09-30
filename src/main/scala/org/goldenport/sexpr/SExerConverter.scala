package org.goldenport.sexpr

import java.net.URL

/*
 * @since   Sep. 25, 2018
 * @version Sep. 25, 2018
 * @author  ASAMI, Tomoharu
 */
sealed trait SExprConverter[T] extends Function1[SExpr, T]

object SExprConverter {
  implicit case object IntConverter extends SExprConverter[Int] {
    def apply(p: SExpr): Int = ???
  }

  implicit case object toStringConverter extends SExprConverter[String] {
    def apply(p: SExpr): String = p.asString
  }

  implicit case object toUrlConverter extends SExprConverter[URL] {
    def apply(p: SExpr): URL = p.asUrl
  }
}
