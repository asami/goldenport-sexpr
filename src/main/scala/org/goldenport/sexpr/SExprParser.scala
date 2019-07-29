package org.goldenport.sexpr

import java.io.Reader
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.LinkedHashMap
import scalax.io.JavaConverters._
import com.asamioffice.goldenport.text.UString
import org.goldenport.sexpr.eval.LogicalTokensReader

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
 *  version Mar. 11, 2015
 *  version Mar. 10, 2016
 *  version Aug. 30, 2018
 *  version Sep.  9, 2018
 *  version Feb. 16, 2019
 *  version Apr. 13, 2019
 * @version Jul. 16, 2019
 * @author  ASAMI, Tomoharu
 */
object SExprParser extends JavaTokenParsers {
  val CACHE_SIZE = 1000
  override protected val whiteSpace = """[\s,]+""".r

  private var _cache = new LinkedHashMap[String, SExpr]()

  def parseWithCache(in: CharSequence): SExpr = {
    val in1 = _normalize(in.toString)
    _get_cache(in1) getOrElse {
      parseAll(sexpr, in1) match {
        case Success(s, _) => _put_cache(in1, s); s
        case Failure(msg, in) => throw new IllegalArgumentException(s"$msg [${in.offset}]: ${in.rest.source}")
        case Error(msg, in) => throw new IllegalArgumentException(s"$msg [${in.offset}]: ${in.rest.source}")
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
      case Failure(msg, in) => throw new IllegalArgumentException(s"$msg [${in.offset}]: ${in.rest.source} in ${in.source}")
      case Error(msg, in) => throw new IllegalArgumentException(s"$msg [${in.offset}]: ${in.rest.source} in ${in.source}")
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
      case number => SNumber(BigDecimal(number))
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

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

object SExprParserNew extends Parsers {
  import org.goldenport.parser._
  import org.goldenport.parser.XmlParser.XmlToken
  import org.goldenport.parser.JsonParser.JsonToken

  type Elem = LogicalToken

  def apply(reader: Reader): SExpr = apply(reader.asReadChars.string)

  def apply(in: CharSequence): SExpr = {
    println(s"apply $in")
    parse(LogicalTokens.parseSexpr(in.toString))
  }

  def parse(in: LogicalToken): SExpr = parse(LogicalTokens(in))

  def parse(in: LogicalTokens): SExpr = {
    println(s"SExprParserNew#parse $in")
    val reader = LogicalTokensReader(in)
    println(reader)
    sexpr(reader) match {
      case Success(s, _) => s
      case Failure(msg, in) => throw new IllegalArgumentException(s"$msg")
      case Error(msg, in) => throw new IllegalArgumentException(s"$msg")
    }
  }

  def sexpr: Parser[SExpr] = nil | list | number | boolean | string | keyword | atom | quote | xml | json | unknown

  def nil = new Parser[SList] {
    def apply(in: Input) = {
      in.first match {
        case AtomToken(s, _) if s == "nil" => Success(SNil, in.rest)
        case m => Failure(s"Not nil: $m", in.rest)
      }
    }
  }

  def list = list0 | listn

  def list0: Parser[SList] = {
    open ~> space.* <~ close ^^ {
      case _ => SNil
    }
  }

  def listn: Parser[SList] = {
    open ~ space.? ~> sexpr ~ delimiter_sexpr.* <~ space.? ~ close ^^ {
      case head ~ tail => SList.create(head +: tail)
    }
  }

  def delimiter_sexpr: Parser[SExpr] = {
    space ~> sexpr ^^ {
      case s => s
    }
  }

  // def delimiter_sexpr: Parser[SExpr] = {
  //   space.* ~ delimiter.? ~ space.* ~> sexpr ^^ {
  //     case s => s
  //   }
  // }

  protected def space = new Parser[SPseudo] {
    def apply(in: Input) = in.first match {
      case m: SpaceToken => Success(SSpace, in.rest)
      case m => Failure(s"Not space: $m", in.rest)
    }
  }

  protected def delimiter = new Parser[SPseudo] {
    def apply(in: Input) = in.first match {
      case DelimiterToken(",", _) => Success(SDelimiter, in.rest)
      case m => Failure(s"Not delimiter: $m", in.rest)
    }
  }

  protected def open = new Parser[SPseudo] {
    def apply(in: Input) = {
      in.first match {
        case DelimiterToken("(", _) => Success(SOpen, in.rest)
        case _ => Failure("Not open", in.rest)
      }
    }
  }

  protected def close = new Parser[SPseudo] {
    def apply(in: Input) = {
      in.first match {
        case DelimiterToken(")", _) => Success(SClose, in.rest)
        case m => Failure(s"Not close: $m", in.rest)
      }
    }
  }

  def number = new Parser[SNumber] {
    def apply(in: Input) = {
      in.first match {
        case NumberToken(n, _) => Success(SNumber(n), in.rest)
        case m => Failure(s"Not number: $m", in.rest)
      }
    }
  }

  def boolean = new Parser[SBoolean] {
    def apply(in: Input) = {
      in.first match {
        case BooleanToken(n, _) => Success(SBoolean(n), in.rest)
        case m => Failure(s"Not boolean: $m", in.rest)
      }
    }
  }

  def string = new Parser[SString] {
    def apply(in: Input) = {
      in.first match {
        case m: StringToken => Success(SString(m.text), in.rest)
        case m => Failure(s"Not string: $m", in.rest)
      }
    }
  }

  def keyword = new Parser[SKeyword] {
    def apply(in: Input) = {
      in.first match {
        case AtomToken(k, _) if k.startsWith(":") => Success(SKeyword(k), in.rest)
        case m => Failure(s"Not keyword: $m", in.rest)
      }
    }
  }

  def atom = new Parser[SAtom] {
    def apply(in: Input) = {
      in.first match {
        case AtomToken(s, _) => Success(SAtom(s), in.rest)
        case m => Failure(s"Not atom: $m", in.rest)
      }
    }
  }

  def quote = {
    single_quote ~> sexpr ^^ {
      case sexpr => SList(SAtom.quote, sexpr)
    }
  }

  def single_quote = new Parser[SAtom] {
    def apply(in: Input) = {
      in.first match {
        case m: SingleQuoteToken => Success(SAtom.quote, in.rest)
        case m => Failure(s"Not quote: $m", in.rest)
      }
    }
  }

  def xml = new Parser[SXml] {
    def apply(in: Input) = {
      in.first match {
        case XmlToken(j, _) => Success(SXml(j), in.rest)
        case m => Failure(s"Not xml: $m", in.rest)
      }
    }
  }

  def json = new Parser[SJson] {
    def apply(in: Input) = {
      in.first match {
        case JsonToken(j, _) => Success(SJson(j), in.rest)
        case m => Failure(s"Not json: $m", in.rest)
      }
    }
  }

  def unknown = new Parser[SExpr] {
    def apply(in: Input) = in.first match {
      case m => Failure(s"Unknown token: $m", in.rest)
    }
  }
}
