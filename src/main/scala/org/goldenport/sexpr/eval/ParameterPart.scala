package org.goldenport.sexpr.eval

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import scala.util.control.NonFatal
import java.nio.charset.Charset
import org.goldenport.RAISE
import org.goldenport.io.InputSource
import org.goldenport.value._
import org.goldenport.record.v2.{Schema}
import org.goldenport.record.v3.{Record, RecordSequence, Table}
import org.goldenport.record.store.Id
import org.goldenport.record.store._
import org.goldenport.record.query.QueryExpression
import org.goldenport.collection.NonEmptyVector
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.entity.{EntityCollection, EntityId, EntityClass}
import Parameters.Cursor

/*
 * @since   Mar. 31, 2019
 *  version Apr. 20, 2019
 *  version May. 14, 2019
 *  version Jul. 14, 2019
 *  version Aug.  3, 2019
 *  version Sep. 30, 2019
 *  version Nov.  9, 2019
 *  version Feb. 29, 2020
 *  version Mar. 30, 2020
 *  version Apr. 12, 2021
 *  version Jun. 13, 2021
 *  version Sep. 19, 2021
 *  version Apr.  9, 2022
 *  version Jul. 31, 2023
 *  version Aug.  1, 2023
varargs *  version Sep. 30, 2023
varargs * @version Sep.  8, 2024
 * @author  ASAMI, Tomoharu
 */
trait ParameterPart { self: LispContext =>
  implicit private val _query_context: Query.Context = sqlContext.queryContext
//  implicit private val _query_context: QueryExpression.Context = sqlContext.queryContext

  object param {
    def cursor(spec: FunctionSpecification) = Cursor(feature, spec, spec.resolve(parameters))

    def cursor(spec: FunctionSpecification, params: Parameters) = Cursor(feature, spec, params)

    def error(p: SError) = State[Cursor, ValidationNel[SError, SExpr]](_.error(p))

    def lift[T](p: T) = State[Cursor, ValidationNel[SError, T]](_.lift(p))

    def argument = State[Cursor, ValidationNel[SError, SExpr]](_.argument)

    def arguments = State[Cursor, ValidationNel[SError, List[SExpr]]](_.arguments)

    def argumentList[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, List[A]]](_.argumentList)

    def argumentNonEmptyVector[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, NonEmptyVector[A]]](_.argumentNonEmptyVector)

    def argument1[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, A]](_.argument1)

    def argumentVarargs[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, List[A]]](_.argumentList)

    def storeCollection = State[Cursor, ValidationNel[SError, Collection]](_.storeCollection)

    def idForStore = State[Cursor, ValidationNel[SError, Id]](_.idForStore)

    def entityCollection = State[Cursor, ValidationNel[SError, EntityCollection]](_.entityCollection)

    def idForEntity = State[Cursor, ValidationNel[SError, EntityId]](_.idForEntity)

    def entityClass = State[Cursor, ValidationNel[SError, EntityClass]](_.entityClass)

    def schema(p: LispContext) = State[Cursor, ValidationNel[SError, Schema]](_.schema(p))

    def query = State[Cursor, ValidationNel[SError, Query]](_.query)

    def queryDefault = State[Cursor, ValidationNel[SError, Query]](_.queryDefault)

    def record = State[Cursor, ValidationNel[SError, Record]](_.record)

    def records = State[Cursor, ValidationNel[SError, RecordSequence]](_.records)

    def powertypeOption[T <: ValueInstance](key: Symbol, powertypeclass: ValueClass[T]) = State[Cursor, ValidationNel[SError, Option[T]]](_.powertypeOption(key, powertypeclass))

    def table(u: LispContext) = State[Cursor, ValidationNel[SError, STable]](_.table(u))

    def tableHeader(u: LispContext) = State[Cursor, ValidationNel[SError, Option[Table.HeaderStrategy]]](_.tableHeader(u))

    def textInFile(u: LispContext) = State[Cursor, ValidationNel[SError, String]](_.textInFile(u))

    def textInFileOr[T](u: LispContext)(f: PartialFunction[Any, T]) = State[Cursor, ValidationNel[SError, Either[String, T]]](_.textInFileOr(u)(f))

    /*
     * Property
     */
    def take(key: Symbol) = State[Cursor, ValidationNel[SError, SExpr]](_.take(key))

    def get(key: Symbol) = State[Cursor, ValidationNel[SError, Option[SExpr]]](_.get(key))

    /*
     * Property Vaue
     */
    def takeString(key: Symbol) = State[Cursor, ValidationNel[SError, String]](_.takeString(key))

    def takeStringStrict(key: Symbol) = State[Cursor, ValidationNel[SError, String]](_.takeStringStrict(key))

    def getInt(key: Symbol) = State[Cursor, ValidationNel[SError, Option[Int]]](_.getInt(key))

    def getString(key: Symbol) = State[Cursor, ValidationNel[SError, Option[String]]](_.getString(key))

    def getStringStrict(key: Symbol) = State[Cursor, ValidationNel[SError, Option[String]]](_.getStringStrict(key))

    def propertyStringList(key: Symbol) = State[Cursor, ValidationNel[SError, List[String]]](_.propertyStringList(key))

    def takeCharset(key: Symbol) = State[Cursor, ValidationNel[SError, Charset]](_.takeCharset(key))

    def getCharset(key: Symbol) = State[Cursor, ValidationNel[SError, Option[Charset]]](
_.getCharset(key))

    def takeInputSource(key: Symbol) = State[Cursor, ValidationNel[SError, InputSource]](_.takeInputSource(key))

    def getInputSource(key: Symbol) = State[Cursor, ValidationNel[SError, Option[InputSource]]](_.getInputSource(key))

    def takeTextInFile(key: Symbol, charset: Charset) = State[Cursor, ValidationNel[SError, String]](_.takeTextInFile(key, charset))

    def takeTextInFile(key: Symbol, charset: ValidationNel[SError, Option[Charset]]) =
      State[Cursor, ValidationNel[SError, String]](_.takeTextInFile(i18nContext, key, charset))

    def getTextInFile(key: Symbol, charset: Charset) = State[Cursor, ValidationNel[SError, Option[String]]](_.getTextInFile(key, charset))

    // case class Cursor(spec: FunctionSpecification, parameters: Parameters) {
    //   def argument1[A](implicit converter: SExprConverter[A]): (Cursor, ValidationNel[SError, A]) =
    //     try {
    //       val r = parameters.argument1(spec)(converter)
    //       val nextspec = spec // TODO
    //       val nextparams = parameters.pop
    //       val nextcursor = Cursor(nextspec, nextparams)
    //       (nextcursor, Success(r))
    //     } catch {
    //       case NonFatal(e) => (this, Failure(SError(e)).toValidationNel)
    //     }

    //   def store: ValidationNel[SError, Store] = RAISE.notImplementedYetDefect

    //   def storeCollection: (Cursor, ValidationNel[SError, Collection]) = {
    //     val store = parameters.getPropertySymbol('store)
    //     val collection = parameters.argument1[Symbol](spec)
    //     val a = feature.store.getCollection(store, collection)
    //     val r = a.map(Success(_)).getOrElse(Failure(SError.notFound("collection", collection.name))).toValidationNel
    //     val spec2 = spec // TODO
    //     (Cursor(spec2, parameters.pop), r)
    //   }

    //   def idForStore: (Cursor, ValidationNel[SError, Id]) = {
    //     val id = parameters.argument1[String](spec)
    //     val r = Success(Id.create(id)).toValidationNel
    //     val spec2 = spec // TODO
    //     (Cursor(spec2, parameters.pop), r)
    //   }

    //   def query: (Cursor, ValidationNel[SError, Query]) = {
    //     val query = parameters.argument1[Query](spec)
    //     val r = Success(query).toValidationNel
    //     val spec2 = spec // TODO
    //     (Cursor(spec2, parameters.pop), r)
    //   }

    //   def record: (Cursor, ValidationNel[SError, Record]) = {
    //     val rec = parameters.argument1[Record](spec)
    //     val r = Success(rec).toValidationNel
    //     val spec2 = spec // TODO
    //     (Cursor(spec2, parameters.pop), r)
    //   }
    // }
  }
}
