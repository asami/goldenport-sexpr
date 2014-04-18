package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._
import org.goldenport.sexpr._

/*
 * @since   Jan.  9, 2014
 *  version Feb.  4, 2014
 * @version Mar. 11, 2014
 * @author  ASAMI, Tomoharu
 */
trait SExprParsers extends Parsers {
  type Elem = SExpr

  protected def atom_name: Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      in.first match {
        case SAtom(name) => Success(name, in.rest)
        case x => Failure("not atom = " + x, in.rest)
      }
    }
  }

  protected def atom(name: String): Parser[SAtom] = new Parser[SAtom] {
    def apply(in: Input) = {
      val NAME = name
      in.first match {
        case x @ SAtom(NAME) => Success(x, in.rest)
        case x => Failure("not atom %s = %s".format(name, x), in.rest)
      }
    }
  }

  protected def boolean: Parser[Boolean] = new Parser[Boolean] {
    def apply(in: Input) = {
      in.first match {
        case SBoolean(s) => Success(s, in.rest)
        case x => Failure("not string = " + x, in.rest)
      }
    }
  }

  protected def string: Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      in.first match {
        case SString(s) => Success(s, in.rest)
        case x => Failure("not string = " + x, in.rest)
      }
    }
  }

  protected def number_long: Parser[Long] = new Parser[Long] {
    def apply(in: Input) = {
      in.first match {
        case SNumber(s) => try {
          Success(s.toLong, in.rest)
        } catch {
          case NonFatal(e) => Failure("not long = " + e, in.rest)
        }
        case x => Failure("not number = " + x, in.rest)
      }
    }
  }

  protected def open = new Parser[SPseudo] {
    def apply(in: Input) = {
      in.first match {
        case SOpen => Success(SOpen, in.rest)
        case _ => Failure("not open", in.rest)
      }
    }
  }

  protected def close = new Parser[SPseudo] {
    def apply(in: Input) = {
      in.first match {
        case SClose => Success(SClose, in.rest)
        case _ => Failure("not close", in.rest)
      }
    }
  }

  protected def atom_boolean(name: String): Parser[Boolean] = {
    // XXX handle keywords
    open ~ atom(name) ~> boolean <~ close
  }

  protected def atom_string(name: String): Parser[String] = {
    // XXX handle keywords
    open ~ atom(name) ~> string <~ close
  }

  protected def atom_long_list(name: String): Parser[Seq[Long]] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_long.* <~ close
  }

  protected def atom_string_list(name: String): Parser[Seq[String]] = {
    // XXX handle keywords
    open ~ atom(name) ~> string.* <~ close
  }

  protected def atom_expr_list(name: String): Parser[Seq[SExpr]] = {
    // XXX handle keywords
    open ~ atom(name) ~> expr_list <~ close ^^ {
      case exprs => exprs
    }
  }

  def keyword(name: String): Parser[SKeyword] = new Parser[SKeyword] {
    def apply(in: Input) = {
      val NAME = name
      in.first match {
        case x @ SKeyword(NAME) => Success(x, in.rest)
        case x => Failure("not keyword %s = %s".format(name, x), in.rest)
      }
    }
  }

  def keyword_boolean(name: String): Parser[Boolean] = {
    keyword(name) ~> boolean
  }

  def keyword_long(name: String): Parser[Long] = {
    keyword(name) ~> number_long
  }

  def keyword_string(name: String): Parser[String] = {
    keyword(name) ~> string
  }

  def keyword_name_boolean(name: String): Parser[(String, Boolean)] = {
    keyword(name) ~> boolean ^^ {
      case v => name -> v
    }
  }

  def keyword_name_long(name: String): Parser[(String, Long)] = {
    keyword(name) ~> number_long ^^ {
      case v => name -> v
    }
  }

  def keyword_name_string(name: String): Parser[(String, String)] = {
    keyword(name) ~> string ^^ {
      case v => name -> v
    }
  }

  protected def as_boolean(
    name: String, params: Seq[(String, Any)],
    v: Boolean
  ): Boolean = {
    get_boolean(name, params) getOrElse v
  }

  protected def get_boolean(name: String, params: Seq[(String, Any)]): Option[Boolean] = {
    params.find(_._1 == name).map {
      case (_, v: Boolean) => v
      case (_, v) => throw new IllegalArgumentException("Illegal boolean %s = %s".format(name, v))
    }
  }

  protected def get_long(name: String, params: Seq[(String, Any)]): Option[Long] = {
    params.find(_._1 == name).map {
      case (_, v: Long) => v
      case (_, v) => throw new IllegalArgumentException("Illegal long %s = %s".format(name, v))
    }
  }

  protected def get_string(name: String, params: Seq[(String, Any)]): Option[String] = {
    params.find(_._1 == name).map {
      case (_, v: String) => v
      case (_, v) => throw new IllegalArgumentException("Illegal string %s = %s".format(name, v))
    }
  }

  protected def expr_list = new Parser[Seq[SExpr]] {
    def apply(in: Input) = {
      val (r, i) = contents(in)
      Success(r, i)
    }
  }

  def contents(in: Input): (Seq[SExpr], Input) = {
    var input = in
    var result = Vector.empty[SExpr]
    var done = false
    while (!done) {
      input.first match {
        case SOpen => {
          val (r, i) = element(input.rest)
          result = result :+ r
          input = i
        }
        case SClose => {
          done = true
        }
        case x => {
          result = result :+ x
          input = input.rest
        }
      }
    }
    (result, input)
  }

  def element(in: Input): (SExpr, Input) = {
    var input = in
    var result = Vector.empty[SExpr]
    var done = false
    while (!done) {
      input.first match {
        case SOpen => {
          val (r, i) = element(input.rest)
          result = result :+ r
          input = i
        }
        case SClose => {
          done = true
          input = input.rest
        }
        case x => {
          result = result :+ x
          input = input.rest
        }
      }
    }
    if (result.isEmpty)
      (SNil, input)
    else
      (make_list(result), input)
  }

  protected def make_list(xs: Seq[SExpr]): SList = {
    xs.foldRight(SNil: SList) { (x, z) => SCell(x, z) }
  }
}
