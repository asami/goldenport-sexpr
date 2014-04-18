package org.goldenport.sexpr

import java.io.Reader
import scala.util.parsing.combinator.JavaTokenParsers
//import scalax.io.{JavaConverters,Codec,LongTraversable,Line}
import scalax.io.JavaConverters._
//import org.goldenport.Strings
import com.asamioffice.goldenport.text.UString

/**
 * @since   Sep.  9, 2012
 *  version Aug.  9, 2013
 *  version Oct. 14, 2013
 *  version Feb.  4, 2014
 *  version Mar. 13, 2014
 * @version Apr. 18, 2014
 * @author  ASAMI, Tomoharu
 */
object SExprParser extends JavaTokenParsers {
  override protected val whiteSpace = """[\s,]+""".r
  def apply(reader: Reader): SExpr = {
    apply(reader.asReadChars.string)
    // parseAll(sexpr, reader) match {
    //   case Success(s, _) => s
    //   case _ => throw new IllegalArgumentException("Illegal sexpr")
    // }
  }

  def apply(in: CharSequence) = {
    val in1 = _remove_comments(in)
    parseAll(sexpr, in1) match {
      case Success(s, _) => s
      case _ => throw new IllegalArgumentException("Illegal sexpr = $in")
    }
  }

  private def _remove_comments(in: CharSequence): CharSequence = {
//    Strings.tolines(in.toString).filter(_.startsWith(";;")).mkString("\n")
    val a = UString.getLineList(in.toString).toVector
    a.filterNot(_.startsWith(";;")).mkString("\n")
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
