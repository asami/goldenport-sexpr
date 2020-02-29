package org.goldenport.sexpr.eval

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import scala.util.control.NonFatal
import org.goldenport.RAISE
import org.goldenport.record.v2.{Schema}
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.store.Id
import org.goldenport.record.store._
import org.goldenport.record.query.QueryExpression
import org.goldenport.collection.NonEmptyVector
import org.goldenport.sexpr._
import org.goldenport.value._
import Parameters.Cursor

/*
 * @since   Mar. 31, 2019
 *  version Apr. 20, 2019
 *  version May. 14, 2019
 *  version Jul. 14, 2019
 *  version Aug.  3, 2019
 *  version Sep. 30, 2019
 *  version Nov.  9, 2019
 * @version Feb. 29, 2020
 * @author  ASAMI, Tomoharu
 */
trait ParameterPart { self: LispContext =>
  implicit private val _query_context: QueryExpression.Context = sqlContext.queryContext

  object param {
    def cursor(spec: FunctionSpecification) = Cursor(feature, spec, parameters)

    def arguments = State[Cursor, ValidationNel[SError, List[SExpr]]](_.arguments)

    def argumentList[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, List[A]]](_.argumentList)

    def argumentNonEmptyVector[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, NonEmptyVector[A]]](_.argumentNonEmptyVector)

    def argument1[A](implicit a: SExprConverter[A]) = State[Cursor, ValidationNel[SError, A]](_.argument1)

    def storeCollection = State[Cursor, ValidationNel[SError, Collection]](_.storeCollection)

    def idForStore = State[Cursor, ValidationNel[SError, Id]](_.idForStore)

    def schema(p: LispContext) = State[Cursor, ValidationNel[SError, Schema]](_.schema(p))

    def query = State[Cursor, ValidationNel[SError, Query]](_.query)

    def queryDefault = State[Cursor, ValidationNel[SError, Query]](_.queryDefault)

    def record = State[Cursor, ValidationNel[SError, Record]](_.record)

    def records = State[Cursor, ValidationNel[SError, RecordSequence]](_.records)

    def powertypeOption[T <: ValueInstance](key: Symbol, powertypeclass: ValueClass[T]) = State[Cursor, ValidationNel[SError, Option[T]]](_.powertypeOption(key, powertypeclass))

    def table(u: LispContext) = State[Cursor, ValidationNel[SError, STable]](_.table(u))

    def propertyStringList(key: Symbol) = State[Cursor, ValidationNel[SError, List[String]]](_.propertyStringList(key))

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
