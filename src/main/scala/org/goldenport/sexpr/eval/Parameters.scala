package org.goldenport.sexpr.eval

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import scala.util.control.NonFatal
import org.goldenport.RAISE
import org.goldenport.record.v2.{Schema}
import org.goldenport.record.v3.{IRecord, Record, RecordSequence}
import org.goldenport.record.store.Id
import org.goldenport.record.store._
import org.goldenport.sexpr._
import org.goldenport.sexpr.SExprConverter._
import org.goldenport.value._

/*
 * @since   Sep. 25, 2018
 *  version Oct. 28, 2018
 *  version Mar. 24, 2019
 *  version Apr.  6, 2019
 *  version May. 21, 2019
 * @version Jul. 25, 2019
 * @author  ASAMI, Tomoharu
 */
case class Parameters(
  arguments: List[SExpr],
  properties: Map[Symbol, SExpr],
  switches: Set[Symbol]
) {
  def show = "Paramerters()" // TODO

  def head: SExpr = argumentOneBased(1)

  def argument(i: Int): SExpr = argumentOneBased(i)

  def argumentZeroBased(i: Int): SExpr = argumentOneBased(i + 1)

  def argumentOneBased(i: Int): SExpr =
    if (arguments.length >= i)
      arguments(i - 1)
    else
      RAISE.invalidArgumentFault(s"Not enough arguments ${i}th (one-based)")

  def getArgumentOneBased(i: Int): Option[SExpr] =
    if (arguments.length >= i)
      Some(arguments(i - 1))
    else
      None

  def argumentList[A](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A]): List[A] = arguments.map(a.apply)

  def argument1[A](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A]) = a.apply(argumentOneBased(1))
  def argument2[A, B](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A], b: SExprConverter[B]): (A, B) = (a.apply(argumentOneBased(1)), b.apply(argumentOneBased(2)))
  def argument3[A, B, C](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A], b: SExprConverter[B], c: SExprConverter[C]): (A, B, C) = (a.apply(argumentOneBased(1)), b.apply(argumentOneBased(2)), c.apply(argumentOneBased(3)))

  def getArgument1[A](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A]): Option[A] = getArgumentOneBased(1).map(a.apply)

  def asStringList: List[String] = arguments.map(_.asString)
  def asBigDecimalList: List[BigDecimal] = arguments.map(_.asBigDecimal)

  def getProperty(p: Symbol): Option[SExpr] = properties.get(p)
  def getPropertyString(p: Symbol): Option[String] = getProperty(p).map {
    case SString(s) => s
    case SAtom(n) => n
    case m => SError.invalidDatatype(p.name, m)
  }
  def getPropertySymbol(p: Symbol): Option[Symbol] = getPropertyString(p).map(Symbol(_))

  def isSwitch(p: Symbol): Boolean = switches.contains(p)

  def map(f: SExpr => SExpr): Parameters = copy(arguments = arguments.map(f))

  def pop: Parameters = copy(arguments = arguments.tail)

  def pop(count: Int): Parameters = copy(arguments = arguments.take(count))
}
object Parameters {
  def apply(p: SList): Parameters = apply(p.list)

  def apply(ps: List[SExpr]): Parameters = {
    case class Z(
      as: Vector[SExpr] = Vector.empty,
      props: Map[Symbol, Vector[SExpr]] = Map.empty,
      switches: Set[Symbol] = Set.empty,
      keyword: Option[Symbol] = None
    ) {
      def r = {
        val ps: Map[Symbol, SExpr] = props.mapValues(xs => xs.length match {
          case 0 => SNil
          case 1 => xs(0)
          case _ => SList.create(xs)
        })
        val ss = keyword.fold(switches)(switches + _)
        Parameters(as.toList, ps, ss)
      }
      def +(rhs: SExpr) = keyword.map(k =>
        rhs match {
          case m: SKeyword => copy(switches = switches + Symbol(m.name), keyword = None)
          case SNil => copy(switches = switches + Symbol(k.name), keyword = None)
          case m => copy(props = props |+| Map(Symbol(k.name) -> Vector(m)), keyword = None)
        }
      ).getOrElse(
        rhs match {
          case m: SKeyword => copy(keyword = Some(Symbol(m.name)))
          case m => copy(as = as :+ m)
        }
      )
    }
    ps./:(Z())(_+_).r
  }

  case class Cursor(feature: FeatureContext, spec: FunctionSpecification, parameters: Parameters) {
    protected def to_result[T](newspec: FunctionSpecification, r: ValidationNel[SError, T]): (Cursor, ValidationNel[SError, T]) =
      (copy(spec = newspec), r)

    protected def to_result_pop[T](newspec: FunctionSpecification, r: ValidationNel[SError, T]): (Cursor, ValidationNel[SError, T]) =
      (copy(spec = newspec, parameters = parameters.pop), r)

    protected def to_result_pop[T](newspec: FunctionSpecification, r: ValidationNel[SError, T], count: Int): (Cursor, ValidationNel[SError, T]) =
      (copy(spec = newspec, parameters = parameters.pop(count)), r)

    protected def to_success[T](newspec: FunctionSpecification, r: T): (Cursor, ValidationNel[SError, T]) =
      (copy(spec = newspec), Success(r))

    protected def to_error[T](newspec: FunctionSpecification, e: SError): (Cursor, ValidationNel[SError, T]) =
      (copy(spec = newspec), Failure(NonEmptyList(e)))

    def argumentList[A](implicit converter: SExprConverter[A]): (Cursor, ValidationNel[SError, List[A]]) =
      try {
        val r = parameters.argumentList(spec)(converter)
        val nextspec = spec // TODO
        val nextparams = parameters.copy(arguments = Nil)
        val nextcursor = Cursor(feature, nextspec, nextparams)
        (nextcursor, Success(r))
      } catch {
        case NonFatal(e) => (this, Failure(SError(e)).toValidationNel)
      }

    def argument1[A](implicit converter: SExprConverter[A]): (Cursor, ValidationNel[SError, A]) =
      try {
        val r = parameters.argument1(spec)(converter)
        val nextspec = spec // TODO
        val nextparams = parameters.pop
        val nextcursor = Cursor(feature, nextspec, nextparams)
        (nextcursor, Success(r))
      } catch {
        case NonFatal(e) => (this, Failure(SError(e)).toValidationNel)
      }

    def store: ValidationNel[SError, Store] = RAISE.notImplementedYetDefect

    def storeCollection: (Cursor, ValidationNel[SError, Collection]) = {
      val store = parameters.getPropertySymbol('store)
      val collection = parameters.argument1[Symbol](spec)
      val a = feature.store.getCollection(store, collection)
      val r = a.map(Success(_)).getOrElse(Failure(SError.notFound("collection", collection.name))).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def idForStore: (Cursor, ValidationNel[SError, Id]) = {
      val id = parameters.argument1[String](spec)
      val r = Success(Id.create(id)).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def schema(p: LispContext): (Cursor, ValidationNel[SError, Schema]) = {
      val name = parameters.argument1[String](spec)
      val n = s"model.voucher.$name"
      val r = p.bindings.get(n).map {
        case m: SSchema => Success(m.schema).toValidationNel
        case m: Schema => Success(m).toValidationNel
        case m => RAISE.notImplementedYetDefect
      }.getOrElse(RAISE.notImplementedYetDefect)
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def query: (Cursor, ValidationNel[SError, Query]) = {
      val query = parameters.argument1[Query](spec)
      val r = Success(query).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def record: (Cursor, ValidationNel[SError, Record]) = {
      val rec = parameters.argument1[Record](spec)
      val r = Success(rec).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def records: (Cursor, ValidationNel[SError, RecordSequence]) = {
      case class Z(rs: Vector[IRecord] = Vector.empty, count: Int = 0, donep: Boolean = false) {
        def records: RecordSequence = RecordSequence(rs)

        def +(rhs: SExpr) = rhs match {
          case m: SRecord => copy(rs :+ m.record, count + 1)
          case m: SCell => _list(m)
          case m => _done
        }

        private def _done = copy(donep = true)

        private def _list(p: SCell) = {
          val xs = p.vector
          if (xs.forall(_.isInstanceOf[SRecord]))
            copy(rs ++ xs.collect { case m: SRecord => m.record }, count + 1)
          else
            _done
        }
      }
      val z = parameters.arguments./:(Z())(_+_)
      val rs = z.records
      val r = Success(rs).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def powertypeOption[T <: ValueInstance](key: Symbol, powertypeclass: ValueClass[T]): (Cursor, ValidationNel[SError, Option[T]]) = {
      val nextspec = spec // TODO
      parameters.getPropertySymbol(key).
        map { x =>
          powertypeclass.get(x.name).
            map(y => to_success(nextspec, Some(y))).
            getOrElse(to_error(nextspec, SError.invalidArgument(key.name, x.toString)))
        }.getOrElse {
          to_success(nextspec, None)
        }
    }
  }
}
