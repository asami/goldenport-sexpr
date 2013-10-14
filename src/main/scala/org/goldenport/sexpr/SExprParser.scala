package org.goldenport.sexpr

import java.io.Reader
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * @since   Sep.  9, 2012
 *  version Aug.  9, 2013
 * @version Oct. 14, 2013
 * @author  ASAMI, Tomoharu
 */
object SExprParser extends JavaTokenParsers {
  override protected val whiteSpace = """[\s,]+""".r
  def apply(reader: Reader): SExpr = {
    parseAll(sexpr, reader) match {
      case Success(s, _) => s
    }
  }
  def apply(in: CharSequence) = {
    parseAll(sexpr, in) match {
      case Success(s, _) => s
    }
  }

  def sexpr: Parser[SExpr] = nil | list | number | boolean | string | keyword | atom

//  def symbol: Parser[String] = """([\w-./]|[^\x01-\x7E])+""".r
  def symbol: Parser[String] = """([\w-./!@#$%&*+=|:;<>]|[^\x01-\x7E])+""".r

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

  def boolean: Parser[SBoolean] = boolean_true | boolean_false
  def boolean_true: Parser[SBoolean] = {
    "true" ^^ {
      case s => SBoolean(true)
    }
  }
  def boolean_false: Parser[SBoolean] = {
    "false" ^^ {
      case s => SBoolean(false)
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
