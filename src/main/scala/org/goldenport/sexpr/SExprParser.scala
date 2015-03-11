package org.goldenport.sexpr

import java.io.Reader
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.LinkedHashMap
import scalax.io.JavaConverters._
import com.asamioffice.goldenport.text.UString

/*
 * @since   Sep.  9, 2012
 *  version Aug.  9, 2013
 *  version Oct. 14, 2013
 *  version Feb.  4, 2014
 *  version Mar. 13, 2014
 *  version Apr. 18, 2014
 *  version Sep. 15, 2014
 *  version Dec. 17, 2014
 *  version Feb.  6, 2015
 * @version Mar. 11, 2015
 * @author  ASAMI, Tomoharu
 */
object SExprParser extends JavaTokenParsers {
  val CACHE_SIZE = 1000
  override protected val whiteSpace = """[\s,]+""".r

  private var _cache = new LinkedHashMap[String, SExpr]()

  def parseWithCaching(in: CharSequence): SExpr = {
    val in1 = _normalize(in.toString)
    _get_cache(in1) getOrElse {
      parseAll(sexpr, in1) match {
        case Success(s, _) => _put_cache(in1, s); s
        case _ => throw new IllegalArgumentException(s"Illegal sexpr = $in")
      }
    }
  }

  private def _get_cache(s: String): Option[SExpr] = {
    _cache.get(s)
  }

  private def _put_cache(s: String, expr: SExpr) {
    _cache.put(s, expr)
    _cache = _cache.take(CACHE_SIZE)
  }

  def apply(reader: Reader): SExpr = {
    apply(reader.asReadChars.string)
    // parseAll(sexpr, reader) match {
    //   case Success(s, _) => s
    //   case _ => throw new IllegalArgumentException("Illegal sexpr")
    // }
  }

  def apply(in: CharSequence): SExpr = {
    val in1 = _normalize(in.toString)
    parseAll(sexpr, in1) match {
      case Success(s, _) => s
      case _ => throw new IllegalArgumentException(s"Illegal sexpr = $in")
    }
  }

  private def _normalize(in: String): String = {
    val a = _remove_comments(in)
    val b = _raw_string_literal(a)
    b
  }

  private def _remove_comments(in: String): String = {
//    Strings.tolines(in.toString).filter(_.startsWith(";;")).mkString("\n")
    val a = UString.getLineList(in.toString).toVector
    a.filterNot(_.startsWith(";;")).mkString("\n")
  }

  private def _raw_string_literal(in: String): String = {
    RawStringLiteralTransformer.transform(in)
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
    myStringLiteral ^^ {
      case string =>
        val a = string.substring(1, string.length - 1)
        // sync with SExpr.toStringLiteral
        val b = a.replace("\\n", "\n").replace("\\r", "\r").
          replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\")
        SString(b)
    }
  }

  def myStringLiteral: Parser[String] = {
    // Scala 2.10.3
    // https://github.com/scala/scala/issues/3017
//    ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
    ("\"" + """([^"\p{Cntrl}\\]*+(?:\\[\\'"bfnrt])*+(?:\\u[a-fA-F0-9]{4})*+)*+""" + "\"").r
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
