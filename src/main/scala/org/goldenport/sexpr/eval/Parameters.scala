package org.goldenport.sexpr.eval

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import scala.util.control.NonFatal
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.context.ConsequenceSequence
import org.goldenport.collection.NonEmptyVector
import org.goldenport.collection.VectorMap
import org.goldenport.record.v2.{Schema}
import org.goldenport.record.v2.{XDecimal}
import org.goldenport.record.v3.{IRecord, Record, RecordSequence, Table}
import org.goldenport.record.v3.Field
import org.goldenport.record.v3.Column
import org.goldenport.record.store.Id
import org.goldenport.record.store._
import org.goldenport.record.query.QueryExpression
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.FunctionSpecification.Parameter.{Argument => ArgumentSpec}
import org.goldenport.sexpr.eval.entity.{EntityCollection, EntityId, EntityClass}
// import org.goldenport.sexpr.SExprConverter._
import org.goldenport.value._

/*
 * @since   Sep. 25, 2018
 *  version Oct. 28, 2018
 *  version Mar. 24, 2019
 *  version Apr.  6, 2019
 *  version May. 21, 2019
 *  version Jul. 25, 2019
 *  version Aug.  3, 2019
 *  version Sep. 30, 2019
 *  version Oct.  5, 2019
 *  version Nov.  8, 2019
 *  version Dec.  7, 2019
 *  version Jan. 26, 2020
 *  version Feb. 29, 2020
 *  version Mar. 30, 2020
 *  version Jan. 16, 2021
 *  version Mar. 21, 2021
 *  version Apr. 12, 2021
 *  version May. 20, 2021
 *  version Jun. 13, 2021
 *  version Sep. 21, 2021
 *  version Apr.  4, 2022
 *  version May.  5, 2022
 *  version Aug. 31, 2022
 *  version Sep.  1, 2022
 * @version Nov.  6, 2022
 * @author  ASAMI, Tomoharu
 */
case class Parameters(
  argumentVector: Vector[Parameters.Argument],
  properties: VectorMap[Symbol, SExpr],
  switches: Set[Symbol]
) {
  def show = "Paramerters()" // TODO

  def isEmptyArguments: Boolean = argumentVector.isEmpty

  def isError: Boolean = isEmptyArguments || isErrorProperties

  def isErrorArguments: Boolean = arguments.exists(_.isInstanceOf[SError])
  def isErrorProperties: Boolean = properties.values.exists(_.isInstanceOf[SError])

  lazy val arguments: List[SExpr] = argumentVector.toList.map(_.value)

  def head: SExpr = argumentOneBased(1)

  def sexpr: SExpr = SList.create(arguments)

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

  def argumentNonEmptyVector[A](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A]): NonEmptyVector[A] =
    NonEmptyVector.createOption(arguments.map(a.apply)).getOrElse(
      RAISE.invalidArgumentFault("Empty list"))

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

  // Unify with FunctionSpecification#resolve
  // from ServiceMoel#ScriptFunction#_eval
  def argumentsUsingProperties(ps: Seq[String]): List[SExpr] =
    argumentVectorUsingProperties(ps).map(_.value).toList

  private def argumentVectorUsingProperties(ps: Seq[String]): Vector[Parameters.Argument] = {
    case class Z(
      args: List[SExpr] = arguments,
      results: Vector[Parameters.Argument] = Vector.empty
    ) {
      def r = {
        val xs: Seq[Parameters.Argument] = args.map(Parameters.Argument.apply)
        results ++ xs
      }

      def +(rhs: String) = getProperty(rhs).
        map(x => copy(results = results :+ Parameters.Argument(rhs, x))).
        getOrElse {
          args match {
            case Nil => this
            case x :: xs => copy(args = xs, results = results :+ Parameters.Argument(rhs, x))
          }
        }
    }
    ps./:(Z())(_+_).r
  }

  def asStringList: List[String] = arguments.map(_.asString)
  def asBigDecimalList: List[BigDecimal] = arguments.map(_.asBigDecimal)

  def getProperty(p: Symbol): Option[SExpr] = properties.get(p) orElse argumentVector.toStream.flatMap {
    case Parameters.NamedArgument(k, v) if p == k => Some(v)
    case _ => None
  }.headOption
  def getProperty(p: String): Option[SExpr] = getProperty(Symbol(p))
  def getPropertyString(p: Symbol): Option[String] = getProperty(p).map {
    case SString(s) => s
    case SAtom(n) => n
    case m => SError.invalidDatatype(p.name, m).RAISE
  }
  def getPropertyStringStrict(p: Symbol): Option[String] = getProperty(p).map {
    case SString(s) => s
    case m => SError.invalidDatatype(p.name, m).RAISE
  }
  def getPropertyStringList(p: Symbol): List[String] = getProperty(p).map {
    case SString(s) => List(s)
    case SAtom(n) => List(n)
    case m: SList => m.list.map {
      case SString(s) => s
      case SAtom(n) => n
      case m => SError.invalidDatatype(p.name, m).RAISE // SError.invalidDatatype(p.name, m)
    }
    case m => SError.invalidDatatype(p.name, m).RAISE // SError.invalidDatatype(p.name, m)
  }.getOrElse(Nil)
  def getPropertySymbol(p: Symbol): Option[Symbol] = getPropertyString(p).map(Symbol(_))
  def getPropertySymbolList(p: Symbol): List[Symbol] = getPropertyStringList(p).map(Symbol(_))
  def getPropertyInt(p: Symbol): Option[Int] = getPropertyIntRigid(p)
  def getPropertyIntRigid(p: Symbol): Option[Int] = getProperty(p).map {
    case SNumber(s) => s.toInt
    case m => SError.invalidDatatype(p.name, m).RAISE
  }
  def getPropertyIntEager(p: Symbol): Option[Int] = getProperty(p).map {
    case SNumber(s) => s.toInt
    case SString(s) => s.toInt
    case m => SError.invalidDatatype(p.name, m).RAISE
  }

  def fetchPropertyString(p: Symbol): ValidationNel[SError, String] =
    fetch_property(p)(_ match {
      case SString(s) => Success(s).toValidationNel
      case SAtom(n) => Success(n).toValidationNel
      case m => Failure(SError.invalidDatatype(p.name, m)).toValidationNel
    })

  def fetchPropertyStringStrict(p: Symbol): ValidationNel[SError, String] =
    fetch_property(p)(_ match {
      case SString(s) => Success(s).toValidationNel
      case m => Failure(SError.invalidDatatype(p.name, m)).toValidationNel
    })

  protected def fetch_property[T](p: Symbol)(body: SExpr => ValidationNel[SError, T]): ValidationNel[SError, T] = 
    getProperty(p).map(body).getOrElse(Failure(SError.notFound(p)).toValidationNel)

  def isSwitch(p: Symbol): Boolean = switches.contains(p)

  def tableHeader: Option[Table.HeaderStrategy] = getPropertyString(Symbol("table-header")).
    map {
      case "name" => Table.HeaderStrategy.name
      case "label" => Table.HeaderStrategy.label
      case m => RAISE.invalidArgumentFault(s"Invalid table-header: $m.")
    }

  def uriSExpr: (SUri, SExpr) = {
    getArgumentOneBased(1).map {
      case m: SUri => (m, argumentOneBased(2))
      case m: SUrl => (m.asSUri, argumentOneBased(2))
      case m: SUrn => (m.asSUri, argumentOneBased(2))
      case m => argumentOneBased(2) match {
        case mm: SUri => (mm, m)
        case mm: SUrl => (mm.asSUri, m)
        case mm: SUrn => (mm.asSUri, m)
        case mm => RAISE.invalidArgumentFault("No SUri is specified.")
      }
    }.getOrElse(
      RAISE.invalidArgumentFault("Two arguments (SUri, SExpr) are required.")
    )
  }

  def map(f: SExpr => SExpr): Parameters = copy(argumentVector = argumentVector.map(_.map(f)))

  def pop: Parameters = copy(argumentVector = argumentVector.tail)

  def pop(count: Int): Parameters = copy(argumentVector = argumentVector.take(count))

  def resolve(p: FunctionSpecification): Parameters = {
    // val paramnames = p.parameters.argumentNames
    // val ps = _parameters_using_arguments(paramnames, as)
    // copy(argumentMap = as, properties = ps)
    // val as = argumentVectorUsingProperties(paramnames)
    // copy(argumentVector = as)
    _resolve_arguments(p)
  }

  private def _resolve_arguments(p: FunctionSpecification): Parameters = {
    case class Z(
      in: Vector[Parameters.Argument] = argumentVector,
      out: ConsequenceSequence[Parameters.Argument] = ConsequenceSequence.empty
    ) {
      def r: Parameters = {
        val a = p.signature.parameters.variableArityArgument match {
          case Some(s) => (out + in.map(_resolve(s, _)))
          case None =>
            if (in.isEmpty)
              out
            else
              out + Consequence.tooManyArgumentsFault(in.map(_.value))
        }
        // for {
        //   as <- a.toConsequence
        //   ps = properties // TODO argument
        // } yield Parameters(as, ps, switches)
        val as: Vector[Parameters.Argument] = a.consequences.map {
          case Consequence.Success(s, _) => s
          case Consequence.Error(c) => Parameters.AnonArgument(SError(c))
        }
        val ps = properties // TODO argument
        Parameters(as, ps, switches)
      }

      def +(rhs: ArgumentSpec) = in.headOption match {
        case Some(s) => copy(in = in.tail, out = out + _resolve(rhs, s))
        case None => this
      }
    }
    p.signature.parameters.arguments./:(Z())(_+_).r
  }

  private def _resolve(c: ArgumentSpec, p: Parameters.Argument): Consequence[Parameters.Argument] =
    _resolve(c.metadata, p)

  private def _resolve(c: Column, p: Parameters.Argument): Consequence[Parameters.Argument] =
    _resolve(c, p.value).map(p.set)

  private def _resolve(c: Column, p: SExpr): Consequence[SExpr] = c.domain.datatype match {
    case XDecimal => _resolve_decimal(c, p)
    case _ => Consequence.success(p)
  }

  private def _resolve_decimal(c: Column, p: SExpr): Consequence[SNumber] = p match {
    case m: SNumber => Consequence.success(m)
    case m: SString => Consequence(SNumber(m.string))
    case m => Consequence.valueDomainFault("Unavailable number", m.show)
  }

  // private def _convert_decimal(c: Column, p: SExpr) = p match {
  //   case m: SAtom => m
  //   case m: SKeyword => m
  //   case m: SNumber => m
  //   case m: SRational => m
  //   case m: SComplex => m
  //   case m: SBoolean => m
  //   case m: SRange => m
  //   case m: SInterval => m
  //   case m: SString => m
  //   case m: SList => m
  //   case m: SLambda => m
  //   case m: SError => m
  //   case m: SException => m
  //   case m: SLongJump => m
  //   case m: SMetaCommand => m
  //   case m: SConsoleOutput => m
  //   case m: SConsequence => m
  //   case m: SBinary => m
  //   case m: SI18NString => m
  //   case m: SI18NTemplate => m
  //   case m: SMessage => m
  //   case m: SRegex => m
  //   case m: SClob => m
  //   case m: SBlob => m
  //   case m: SDocument => m
  //   case m: SSlip => m
  //   case m: SVoucher => m
  //   case m: SEntity => m
  //   case m: SSchema => m
  //   case m: SQuery => m
  //   case m: SRecord => m
  //   case m: STable => m
  //   case m: SVector => m
  //   case m: SMatrix => m
  //   case m: SDataFrame => m
  //   case m: SLxsv => m
  //   case m: SUrl => m
  //   case m: SUrn => m
  //   case m: SUri => m
  //   case m: SExpression => m
  //   case m: SScript => m
  //   case m: SProcess => m
  //   case m: SWindow => m
  //   case m: SSingleQuote => m
  //   case m: SBean => m
  //   case m: SXml => m
  //   case m: SHtml => m
  //   case m: SXPath => m
  //   case m: SXsl => m
  //   case m: SPug => m
  //   case m: SJson => m
  //   case m: SHocon => m
  //   case m: SDateTime => m
  //   case m: SLocalDateTime => m
  //   case m: SLocalDate => m
  //   case m: SLocalTime => m
  //   case m: SMonthDay => m
  //   case m: SDateTimeInterval => m
  //   case m: SLocalDateTimeInterval => m
  //   case m: SDuration => m
  //   case m: SPeriod => m
  //   case m: SImage => m
  //   case m: SMoney => m
  //   case m: SPercent => m
  //   case m: SUnit => m
  //   case m: SChart => m
  //   case m: SChartSpace => m
  //   case m: SChartSeries => m
  //   case m: SMute => m
  //   case m: SFuture => m
  //   case m: SLazy => m
  //   case m: SLazyFuture => m
  //   case SOpen => p
  //   case SClose => p
  //   case SSpace => p
  //   case SDelimiter => p
  // }

  def restore: List[SExpr] = switches.toList.map(_to_switch) ::: properties.toList.flatMap(_to_property) ::: arguments

  private def _to_switch(p: Symbol): SExpr = SKeyword(p.name)

  private def _to_property(p: (Symbol, SExpr)): List[SExpr] = List(SKeyword(p._1.name), p._2)
  // private def _parameters_using_arguments(
  //   paramnames: List[String],
  //   args: VectorMap[Symbol, Parameters.Argument]
  // ): Map[Symbol, SExpr] = {
  //   case class Z(
  //     props: Map[Symbol, SExpr] = properties
  //   ) {
  //     def r = ??? // props

  //     def +(rhs: String) = {
  //       val k = Symbol(rhs)
  //       if (props.contains(k))
  //         this
  //       else
  //         args.headOption.
  //           map(x => copy(props = props + (k -> x))).
  //           getOrElse(this)
  //     }
  //   }
  //   paramnames./:(Z())(_+_).r
  // }
}

object Parameters {
  sealed trait Argument {
    def value: SExpr
    def map(f: SExpr => SExpr): Argument
    def set(v: SExpr): Argument
  }
  case class NamedArgument(key: Symbol, value: SExpr) extends Argument {
    def map(f: SExpr => SExpr) = copy(value = f(value))
    def set(v: SExpr): NamedArgument = copy(value = v)
  }
  case class AnonArgument(value: SExpr) extends Argument {
    def map(f: SExpr => SExpr) = copy(value = f(value))
    def set(v: SExpr): AnonArgument = copy(value = v)
  }
  object Argument {
    def apply(value: SExpr): Argument = AnonArgument(value)
    def apply(name: String, value: SExpr): Argument = NamedArgument(Symbol(name), value)
  }

  def apply(p: SList): Parameters = apply(p.list)

  def apply(ps: List[SExpr]): Parameters = {
    case class Z(
      args: Vector[SExpr] = Vector.empty,
      props: VectorMap[Symbol, Vector[SExpr]] = VectorMap.empty,
      switches: Set[Symbol] = Set.empty,
      keyword: Option[Symbol] = None
    ) {
      def r = {
        val ps: VectorMap[Symbol, SExpr] = props.mapValues(xs => xs.length match {
          case 0 => SNil
          case 1 => xs(0)
          case _ => SList.create(xs)
        })
        val ss = keyword.fold(switches)(switches + _)
        val as = args.map(Argument.apply)
        Parameters(as, ps, ss)
      }
      def +(rhs: SExpr) = keyword.map(k =>
        rhs match {
          case m: SKeyword => copy(switches = switches + Symbol(m.name), keyword = None)
          case SNil => copy(switches = switches + Symbol(k.name), keyword = None)
          case m => copy(props = props |+| VectorMap(Symbol(k.name) -> Vector(m)), keyword = None)
        }
      ).getOrElse(
        rhs match {
          case m: SKeyword => copy(keyword = Some(Symbol(m.name)))
          case m => copy(args = args :+ m)
        }
      )
    }
    ps./:(Z())(_+_).r
  }

  def fromExpression(ps: SList): Parameters = fromExpression(ps.list)

  def fromExpression(ps: List[SExpr]): Parameters = ps match {
    case Nil => apply(Nil)
    case x :: xs => apply(xs)
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

    protected def to_error[T](e: SError): (Cursor, ValidationNel[SError, T]) =
      (this, Failure(NonEmptyList(e)))

    protected def run_cursor[T](body: => ValidationNel[SError, T]): (Cursor, ValidationNel[SError, T]) = try {
      val r = body
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    } catch {
      case SError.SErrorException(e) =>
        val nextspec = spec // TODO
        to_result_pop(nextspec, Failure(e).toValidationNel)
      case NonFatal(e) => 
        val nextspec = spec // TODO
        to_result_pop(nextspec, Failure(SError(e)).toValidationNel)
    }

    def error(p: SError): (Cursor, ValidationNel[SError, SExpr]) = RAISE.notImplementedYetDefect

    def lift[A](p: A): (Cursor, ValidationNel[SError, A]) = RAISE.notImplementedYetDefect

    def argument: (Cursor, ValidationNel[SError, SExpr]) = try {
      parameters.arguments match {
        case Nil => to_error(spec, SError.notFound("empty"))
        case x :: xs => 
          val r = Success(x).toValidationNel
          val nextspec = spec // TODO
          to_result_pop(nextspec, r)
      }
    } catch {
      case NonFatal(e) => to_error(SError(e))
    }

    def arguments: (Cursor, ValidationNel[SError, List[SExpr]]) = argumentList[SExpr]

    def argumentList[A](implicit converter: SExprConverter[A]): (Cursor, ValidationNel[SError, List[A]]) =
      try {
        val r = parameters.argumentList(spec)(converter)
        val nextspec = spec // TODO
        val nextparams = parameters.copy(argumentVector = Vector.empty)
        val nextcursor = Cursor(feature, nextspec, nextparams)
        (nextcursor, Success(r))
      } catch {
        case NonFatal(e) => (this, Failure(SError(e)).toValidationNel)
      }

    def argumentNonEmptyVector[A](implicit converter: SExprConverter[A]): (Cursor, ValidationNel[SError, NonEmptyVector[A]]) =
      try {
        val r = parameters.argumentNonEmptyVector(spec)(converter)
        val nextspec = spec // TODO
        val nextparams = parameters.copy(argumentVector = Vector.empty)
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

    def entityCollection: (Cursor, ValidationNel[SError, EntityCollection]) = {
      val entity = parameters.getPropertySymbol('entity)
      val collection = parameters.argument1[Symbol](spec)
      val a = feature.entity.getCollection(entity, collection)
      val r = a.map(Success(_)).getOrElse(Failure(SError.notFound("collection", collection.name))).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def entityClass: (Cursor, ValidationNel[SError, EntityClass]) = {
      ???
    }

    def idForEntity: (Cursor, ValidationNel[SError, EntityId]) = {
      val id = parameters.argument1[String](spec)
      val r = Success(feature.entity.createId(id)).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def schema(p: LispContext): (Cursor, ValidationNel[SError, Schema]) = {
      val r = parameters.arguments(0) match {
        case SString(name) => _schema(p, name)
        case SAtom(name) => _schema(p, name)
        case m: SSchema => Success(m.schema).toValidationNel
        case m: Schema => Success(m).toValidationNel
        case m => RAISE.notImplementedYetDefect(s"Parameters#schema: $m")
      }
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    private def _schema(p: LispContext, name: String) =
      p.bindings.get(name).orElse(_schema_in_binding(p.bindings, name)).map {
        case m: SSchema => Success(m.schema).toValidationNel
        case m: Schema => Success(m).toValidationNel
        case m => RAISE.notImplementedYetDefect
      }.getOrElse(RAISE.notImplementedYetDefect)

    private def _schema_in_binding(p: IRecord, name: String): Option[Any] =
      p.get(s"model.schema.$name") orElse p.get(s"model.voucher.$name")

    def query(implicit context: QueryExpression.Context): (Cursor, ValidationNel[SError, Query]) = {
      val query = parameters.arguments(0) match {
        case SQuery(s) => s
        case SString(s) => QueryFactory.unmarshall(s)
        case m: SJson => QueryFactory.unmarshall(m.text)
        case m: SXml => QueryFactory.unmarshall(m.text)
        case m: SHtml => QueryFactory.unmarshall(m.text)
        case m: SExpr => QueryFactory.unmarshall(m)
        case m => RAISE.invalidArgumentFault(s"Not query: $m")
      }
      val r = Success(query).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def queryDefault(implicit context: QueryExpression.Context): (Cursor, ValidationNel[SError, Query]) =
      if (parameters.isEmptyArguments) {
        val r = Success(Query.all).toValidationNel
        val nextspec = spec // TODO
        to_result(nextspec, r)
      } else {
        query
      }

    def record: (Cursor, ValidationNel[SError, Record]) = {
      val rec = parameters.argument1[Record](spec)
      val r = Success(rec).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    // private def _validation_nel(p: IRecord): ValidationNel[SError, Record] =
    //   Success(p.toRecord).toValidationNel

    // private def _to_record(p: SExpr): ValidationNel[SError, Record] = p match {
    //   case m: SRecord => _validation_nel(m.record)
    //   case m: SCell => _list_to_record(m)
    //   case m => Failure(SError.invalidDatatype("record", s"Not record: ${p.embed}")).toValidationNel
    // }

    // private def _list_to_record(p: SCell) = {
    //   case class Z(
    //     xs: Vector[Field] = Vector.empty,
    //     errors: Vector[SError] = Vector.empty
    //   ) {
    //     def r = errors.headOption match {
    //       case Some(s) => Failure(NonEmptyList.nel(s, errors.tail.toList))
    //       case None => 
    //         _validation_nel(
    //           if (xs.isEmpty)
    //             Record.empty
    //           else
    //             Record(xs)
    //       )
    //     }

    //     def +(rhs: SExpr) = rhs match {
    //       case SCell(car, cdr) => 
    //       case m => _error(m)
    //     }

    //     private def _error(p: SExpr) = copy(errors = errors :+ SError.invalidDatatype(s"Not cell: ${p.embed}"))
    //   }
    //   p.list./:(Z())(_+_).r
    // }

    def records: (Cursor, ValidationNel[SError, RecordSequence]) = {
      case class Z(rs: Vector[IRecord] = Vector.empty, count: Int = 0, donep: Boolean = false) {
        def records: RecordSequence = RecordSequence(rs)

        def +(rhs: SExpr) = rhs match {
          case m: SRecord => copy(rs :+ m.record, count + 1)
          case m: STable => _vector(m.vector.vector)
          case m: SCell => _list(m)
          case m: SVector => _vector(m.vector)
          case m => _done
        }

        private def _list(p: SCell) = _vector(p.vector)

        private def _vector(xs: Seq[_]) = {
          if (xs.forall(_.isInstanceOf[SRecord]))
            copy(rs ++ xs.collect { case m: SRecord => m.record }, count + 1)
          else
            _done
        }

        private def _done = copy(donep = true)
      }
      val z = parameters.arguments./:(Z())(_+_)
      val rs = z.records
      val r = Success(rs).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    // TODO unordered parameters
    def table(u: LispContext): (Cursor, ValidationNel[SError, STable]) = {
      val t = parameters.arguments(0) match {
        case m: STable => m
        case m: SMatrix => m.table
        case SUrl(url) => u.loadTable(url)
        case SUri(uri) => u.loadTable(uri)
        case m => RAISE.invalidArgumentFault(s"Not table: $m")
      }
      val r = Success(t).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r)
    }

    def tableHeader(u: LispContext): (Cursor, ValidationNel[SError, Option[Table.HeaderStrategy]]) = {
      val x = parameters.tableHeader
      val r = Success(x).toValidationNel
      val nextspec = spec // TODO
      to_result(nextspec, r)
    }

    /*
     * Property
     */
    def take(key: Symbol): (Cursor, ValidationNel[SError, SExpr]) = {
      val r = parameters.getProperty(key) match {
        case Some(s) => Success(s).toValidationNel
        case None => Failure(SError.missingArgumentFault(key.name)).toValidationNel
      }
      val nextspec = spec // TODO
      to_result(nextspec, r)
    }

    def get(key: Symbol): (Cursor, ValidationNel[SError, Option[SExpr]]) = {
      val x = parameters.getProperty(key)
      val r = Success(x).toValidationNel
      val nextspec = spec // TODO
      to_result(nextspec, r)
    }

    /*
     * Property Value
     */
    def takeString(key: Symbol): (Cursor, ValidationNel[SError, String]) = run_cursor {
      parameters.fetchPropertyString(key)
    }

    def takeStringStrict(key: Symbol): (Cursor, ValidationNel[SError, String]) = run_cursor {
      parameters.fetchPropertyStringStrict(key)
    }

    def getString(key: Symbol): (Cursor, ValidationNel[SError, Option[String]]) = {
      val x = parameters.getPropertyString(key)
      val r = Success(x).toValidationNel
      val nextspec = spec // TODO
      to_result(nextspec, r)
    }

    def getStringStrict(key: Symbol): (Cursor, ValidationNel[SError, Option[String]]) = {
      val x = parameters.getPropertyStringStrict(key)
      val r = Success(x).toValidationNel
      val nextspec = spec // TODO
      to_result(nextspec, r)
    }

    def getInt(key: Symbol): (Cursor, ValidationNel[SError, Option[Int]]) = {
      val x = parameters.getPropertyInt(key)
      val r = Success(x).toValidationNel
      val nextspec = spec // TODO
      to_result_pop(nextspec, r) // TODO to_result
    }

    def propertyStringList(key: Symbol): (Cursor, ValidationNel[SError, List[String]]) = {
      val x = parameters.getPropertyStringList(key)
      val r = Success(x).toValidationNel
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
  object Cursor {
    // def arguments = State[Cursor, ValidationNel[SError, List[SExpr]]](_.arguments)

    // def argumentList[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, List[A]]](_.argumentList)

    // def argumentNonEmptyVector[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, NonEmptyVector[A]]](_.argumentNonEmptyVector)

    // def argument1[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, A]](_.argument1)

    // def storeCollection = State[Cursor, ValidationNel[SError, Collection]](_.storeCollection)

    // def idForStore = State[Cursor, ValidationNel[SError, Id]](_.idForStore)

    // def schema(p: LispContext) = State[Cursor, ValidationNel[SError, Schema]](_.schema(p))

    // def query = State[Cursor, ValidationNel[SError, Query]](_.query)

    // def queryDefault = State[Cursor, ValidationNel[SError, Query]](_.queryDefault)

    // def record = State[Cursor, ValidationNel[SError, Record]](_.record)

    // def records = State[Cursor, ValidationNel[SError, RecordSequence]](_.records)

    // def powertypeOption[T <: ValueInstance](key: Symbol, powertypeclass: ValueClass[T]) = State[Cursor, ValidationNel[SError, Option[T]]](_.powertypeOption(key, powertypeclass))

    // def table(u: LispContext) = State[Cursor, ValidationNel[SError, STable]](_.table(u))

    // def tableHeader(u: LispContext) = State[Cursor, ValidationNel[SError, Option[Table.HeaderStrategy]]](_.tableHeader(u))

    // def propertyStringList(key: Symbol) = State[Cursor, ValidationNel[SError, List[String]]](_.propertyStringList(key))
  }
}
