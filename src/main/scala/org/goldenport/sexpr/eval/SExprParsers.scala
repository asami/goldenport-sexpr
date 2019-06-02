package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._
import org.goldenport.sexpr._
import org.goldenport.sexpr.util.AnyUtils

/*
 * @since   Jan.  9, 2014
 *  version Feb.  4, 2014
 *  version Mar. 11, 2014
 *  version Apr. 23, 2014
 *  version May. 25, 2014
 *  version Nov. 28, 2014
 *  version Dec.  6, 2015
 *  version Feb. 27, 2016
 *  version Apr. 20, 2019
 * @version May.  9, 2019
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
        case m => Failure(s"no match: $m", in.rest)
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

  protected def atom_long_list(name: String): Parser[List[Long]] = {
    // XXX handle keywords
    open ~ atom(name) ~> number_long.* <~ close
  }

  protected def atom_string_list(name: String): Parser[List[String]] = {
    // XXX handle keywords
    open ~ atom(name) ~> string.* <~ close
  }

  protected def atom_expr_list(name: String): Parser[List[SExpr]] = {
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

  protected def atom_name_long_list(name: String): Parser[(String, List[Long])] = {
    atom_long_list(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_string_list(name: String): Parser[(String, List[String])] = {
    atom_string_list(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_expr_list(name: String): Parser[(String, List[SExpr])] = {
    atom_expr_list(name) ^^ {
      case v => name -> v
    }
  }

  protected def atom_name_atom_name(name: String): Parser[(String, String)] = {
    atom_atom_name(name) ^^ {
      case v => name -> v
    }
  }

  def keyword_name: Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      in.first match {
        case m: SKeyword => Success(m.name, in.rest)
        case m => Failure(s"not keyword: $m", in.rest)
      }
    }
  }

  def keyword_name_expr: Parser[(String, SExpr)] = {
    keyword_name ~ expr ^^ {
      case name ~ expr => (name, expr)
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

  def cell_key_value: Parser[(String, SExpr)] = new Parser[(String, SExpr)] {
    def apply(in: Input) = {
      in.first match {
        case SCell(car, cdr) => car match {
          case SAtom(a) => Success(a -> cdr, in.rest)
          case SString(s) => Success(s -> cdr, in.rest)
          case m => Failure("not key value cell: $m", in.rest)
        }
        case m => Failure("not key value cell: $m", in.rest)
      }
    }

    def value(p: SExpr) = p match {
      case SCell(car, cdr) => cdr match {
        case SNil => car
        case _ => p
      }
      case m => m
    }
  }

  def association_pair_key_value: Parser[(String, SExpr)] = new Parser[(String, SExpr)] {
    def apply(in: Input) = {
      in.first match {
        case SCell(car, cdr) => car match {
          case SAtom(a) => Success(a -> cdr, in.rest)
          case SString(s) => Success(s -> cdr, in.rest)
          case m => Failure("not key value cell: $m", in.rest)
        }
        case m => Failure("not key value cell: $m", in.rest)
      }
    }
  }

  //
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

  protected def as_int(
    name: String, params: Seq[(String, Any)],
    v: Int
  ): Int = {
    get_int(name, params) getOrElse v
  }

  protected def get_int(name: String, params: Seq[(String, Any)]): Option[Int] = {
    params.find(_._1 == name).map {
      case (_, v: Int) => v
      case (_, v) => throw new IllegalArgumentException("Illegal int %s = %s".format(name, v))
    }
  }

  protected def get_int_list(name: String, params: Seq[(String, Any)]): List[Int] = {
    get_int_list_option(name, params) getOrElse Nil
  }

  protected def get_int_list_option(name: String, params: Seq[(String, Any)]): Option[List[Int]] = {
    params.find(_._1 == name).map {
      case (_, xs: Seq[_]) => xs.map(AnyUtils.toInt).toList
      case (_, x) => List(AnyUtils.toInt(x))
    }
  }

  protected def get_int_vector(name: String, params: Seq[(String, Any)]): Vector[Int] = {
    get_int_vector_option(name, params) getOrElse Vector.empty
  }

  protected def get_int_vector_option(name: String, params: Seq[(String, Any)]): Option[Vector[Int]] = {
    params.find(_._1 == name).map {
      case (_, xs: Seq[_]) => xs.map(AnyUtils.toInt).toVector
      case (_, x) => Vector(AnyUtils.toInt(x))
    }
  }

  protected def as_long(
    name: String, params: Seq[(String, Any)],
    v: Long
  ): Long = {
    get_long(name, params) getOrElse v
  }

  protected def get_long(name: String, params: Seq[(String, Any)]): Option[Long] = {
    params.find(_._1 == name).map {
      case (_, v: Long) => v
      case (_, v) => throw new IllegalArgumentException("Illegal long %s = %s".format(name, v))
    }
  }

  protected def get_long_list(name: String, params: Seq[(String, Any)]): List[Long] = {
    get_long_list_option(name, params) getOrElse Nil
  }

  protected def get_long_list_option(name: String, params: Seq[(String, Any)]): Option[List[Long]] = {
    params.find(_._1 == name).map {
      case (_, xs: Seq[_]) => xs.map(AnyUtils.toLong).toList
      case (_, x) => List(AnyUtils.toLong(x))
    }
  }

  protected def get_long_vector(name: String, params: Seq[(String, Any)]): Vector[Long] = {
    get_long_vector_option(name, params) getOrElse Vector.empty
  }

  protected def get_long_vector_option(name: String, params: Seq[(String, Any)]): Option[Vector[Long]] = {
    params.find(_._1 == name).map {
      case (_, xs: Seq[_]) => xs.map(AnyUtils.toLong).toVector
      case (_, x) => Vector(AnyUtils.toLong(x))
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
 
  protected def get_string_list(name: String, params: Seq[(String, Any)]): List[String] =
    get_string_vector(name, params).toList

  protected def get_string_vector(name: String, params: Seq[(String, Any)]): Vector[String] =
    params.filter(_._1 == name).toVector.
      flatMap(x => _to_string_vector(x._2))

  private def _to_string_vector(p: Any): Vector[String] =
    p match {
      case m: Seq[_] => m.toVector.flatMap(_to_string_vector)
      case m => Vector(m.toString)
    }

  protected def expr = new Parser[SExpr] {
    def apply(in: Input) = Success(in.first, in.rest)
  }

  protected def expr_list = new Parser[List[SExpr]] {
    def apply(in: Input) = {
      val (r, i) = contents(in)
      Success(r.toList, i)
    }
  }

  protected def expr_vector = new Parser[Vector[SExpr]] {
    def apply(in: Input) = {
      val (r, i) = contents(in)
      Success(r.toVector, i)
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
