package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._
import org.goldenport.sexpr._

/*
 * @since   Jan.  9, 2014
 * @version Mar.  4, 2014
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
        case x => Failure("not atom = " + x, in.rest)
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
