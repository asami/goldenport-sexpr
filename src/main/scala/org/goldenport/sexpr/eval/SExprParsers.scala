package org.goldenport.sexpr.eval

import org.goldenport.sexpr._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

/*
 * @since   Jan.  9, 2014
 * @version Jan.  9, 2014
 * @author  ASAMI, Tomoharu
 */
trait SExprParsers extends Parsers {
  type Elem = SExpr

  def atomName: Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      in.first match {
        case SAtom(name) => Success(name, in.rest)
        case _ => Failure("not atom", in.rest)
      }
    }
  }

  def atom(name: String): Parser[SAtom] = new Parser[SAtom] {
    def apply(in: Input) = {
      val NAME = name
      in.first match {
        case x @ SAtom(NAME) => Success(x, in.rest)
        case _ => Failure("not atom", in.rest)
      }
    }
  }

  def str: Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      in.first match {
        case SString(s) => Success(s, in.rest)
        case _ => Failure("not atom", in.rest)
      }
    }
  }
}
