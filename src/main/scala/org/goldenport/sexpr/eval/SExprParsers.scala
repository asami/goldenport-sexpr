package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._
import org.goldenport.sexpr._

/*
 * @since   Jan.  9, 2014
 *  version Feb.  4, 2014
 *  version Mar. 11, 2014
 *  version Apr. 23, 2014
 * @version May. 25, 2014
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

  protected def number_byte: Parser[Byte] = new Parser[Byte] {
    def apply(in: Input) = {
      in.first match {
        case SNumber(s) => try {
          Success(s.toByte, in.rest)
        } catch {
          case NonFatal(e) => Failure("not byte = " + e, in.rest)
        }
        case x => Failure("not number = " + x, in.rest)
      }
    }
  }
  protected def number_short: Parser[Short] = new Parser[Short] {
    def apply(in: Input) = {
      in.first match {
        case SNumber(s) => try {
          Success(s.toShort, in.rest)
        } catch {
          case NonFatal(e) => Failure("not short = " + e, in.rest)
        }
        case x => Failure("not number = " + x, in.rest)
      }
    }
  }
  protected def number_int: Parser[Int] = new Parser[Int] {
    def apply(in: Input) = {
      in.first match {
        case SNumber(s) => try {
          Success(s.toInt, in.rest)
        } catch {
          case NonFatal(e) => Failure("not int = " + e, in.rest)
        }
        case x => Failure("not number = " + x, in.rest)
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

  protected def number_float: Parser[Float] = new Parser[Float] {
    def apply(in: Input) = {
      in.first match {
        case SNumber(s) => try {
          Success(s.toFloat, in.rest)
        } catch {
          case NonFatal(e) => Failure("not float = " + e, in.rest)
        }
        case x => Failure("not number = " + x, in.rest)
      }
    }
  }

  protected def number_double: Parser[Double] = new Parser[Double] {
    def apply(in: Input) = {
      in.first match {
        case SNumber(s) => try {
          Success(s.toDouble, in.rest)
        } catch {
          case NonFatal(e) => Failure("not double = " + e, in.rest)
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

  protected def atom_atom_name(name: String): Parser[String] = {
    // XXX handle keywords
    open ~ atom(name) ~> atom_name <~ close
  }

  protected def atom_boolean(name: String): Parser[Boolean] = {
    // XXX handle keywords
    open ~ atom(name) ~> boolean <~ close
  }

  protected def atom_byte(name: String): Parser[Byte] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_byte <~ close
  }

  protected def atom_short(name: String): Parser[Short] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_short <~ close
  }

  protected def atom_int(name: String): Parser[Int] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_int <~ close
  }

  protected def atom_long(name: String): Parser[Long] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_long <~ close
  }

  protected def atom_float(name: String): Parser[Float] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_float <~ close
  }

  protected def atom_double(name: String): Parser[Double] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_double <~ close
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

  protected def atom_name_boolean(name: String): Parser[(String, Boolean)] = {
    atom_boolean(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_byte(name: String): Parser[(String, Byte)] = {
    atom_byte(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_short(name: String): Parser[(String, Short)] = {
    atom_short(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_int(name: String): Parser[(String, Int)] = {
    atom_int(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_long(name: String): Parser[(String, Long)] = {
    atom_long(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_float(name: String): Parser[(String, Float)] = {
    atom_float(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_double(name: String): Parser[(String, Double)] = {
    atom_double(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_string(name: String): Parser[(String, String)] = {
    atom_string(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_long_list(name: String): Parser[(String, Seq[Long])] = {
    atom_long_list(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_string_list(name: String): Parser[(String, Seq[String])] = {
    atom_string_list(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_expr_list(name: String): Parser[(String, Seq[SExpr])] = {
    atom_expr_list(name) ^^ {
      case v => name -> v
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

  protected def as_string(name: String, params: Seq[(String, Any)]): String = {
    get_string(name, params) getOrElse {
      throw new NoSuchElementException(s"String property $name")
    }
  }

  protected def get_string(name: String, params: Seq[(String, Any)]): Option[String] = {
    params.find(_._1 == name).map {
      case (_, v: String) => v
      case (_, v) => throw new IllegalArgumentException("Illegal string %s = %s".format(name, v))
    }
  }
 
  protected def get_string_vector(name: String, params: Seq[(String, Any)]): Vector[String] = {
    params.filter(_._1 == name).map(_._2.toString).toVector
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
