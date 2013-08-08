package org.goldenport.sexpr

import java.io.Reader
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * @since   Sep.  9, 2012
 * @version Aug.  8, 2013
 * @author  ASAMI, Tomoharu
 */
object SExprParser extends JavaTokenParsers {
  override protected val whiteSpace = """[\s,]+""".r
  def apply(reader: Reader) = parseAll(sexpr, reader)
  def apply(in: CharSequence) = parseAll(sexpr, in)

  def sexpr: Parser[SExpr] = nil | list | number | string | keyword | atom

  def symbol: Parser[String] = """[\w-]+""".r

  def atom: Parser[SAtom] = {
    symbol ^^ {
      case name => SAtom(name)
    }
  }

  def keyword: Parser[SKeyword] = {
    ":"~>symbol ^^ {
      case name => SKeyword(name)
    }
  }

  def number: Parser[SNumber] = {
    wholeNumber ^^ {
      case number => SNumber(number)
    }
  }

  def string: Parser[SString] = {
    stringLiteral ^^ {
      case string => SString(string.substring(1, string.length - 1))
    }
  }


  def nil: Parser[SList] = {
    "nil" ^^ {
      case _ => SNil
    }
  }

  def list: Parser[SList] = {
    "(" ~> rep(sexpr) <~ ")" ^^ {
      case Nil => SNil
      case xs => xs.foldRight(SNil: SList)((x, a) => SCell(x, a))
    }
  }
}
