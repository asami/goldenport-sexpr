package org.goldenport.sexpr.eval.store

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import org.goldenport.RAISE
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.store._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Mar. 30, 2019
 *  version Apr. 14, 2019
 *  version Jul. 14, 2019
 *  version Aug.  2, 2019
 *  version Oct.  5, 2019
 * @version Nov. 27, 2019
 * @author  ASAMI, Tomoharu
 */
object StoreFunction {
  val functions = Vector(
    StoreGet, StoreSelect, StoreInsert, StoreUpdate, StoreDelete,
    StoreCreate, StoreDrop, StoreDefine
  )

  case object StoreGet extends IoFunction {
    val specification = FunctionSpecification("store-get", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
        id <- p.param.idForStore
      } yield {
        (collection |@| id)(p.feature.store.get(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreSelect extends IoFunction {
    val specification = FunctionSpecification("store-select", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
        query <- p.param.queryDefault
      } yield {
        (collection |@| query)(p.feature.store.select(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreInsert extends SyncIoFunction { // TODO IoFunction and synchronize in the connection context.
    val specification = FunctionSpecification("store-insert", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
        rs <- p.param.records
      } yield {
        (collection |@| rs)(p.feature.store.insert(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreUpdate extends SyncIoFunction { // TODO IoFunction and synchronize in the connection context.
    val specification = FunctionSpecification("store-update", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
        id <- p.param.idForStore
        rec <- p.param.record
      } yield {
        (collection |@| id |@| rec)(p.feature.store.update(_, _, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreDelete extends SyncIoFunction { // TODO IoFunction and synchronize in the connection context.
    val specification = FunctionSpecification("store-delete", 2)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
        id <- p.param.idForStore
      } yield {
        (collection |@| id)(p.feature.store.delete(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreCreate extends SyncIoFunction {
    val specification = FunctionSpecification("store-create", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
      } yield {
        collection.map(p.feature.store.create(_)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreCreateOld extends SyncIoFunction {
    val specification = FunctionSpecification("store-create", 2)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.argument1[Symbol]
        schema <- p.param.schema(p)
      } yield {
        (collection |@| schema)(p.feature.store.create(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreDrop extends SyncIoFunction { // TODO IoFunction and synchronize in the connection context.
    val specification = FunctionSpecification("store-drop", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.storeCollection
      } yield {
        collection.map(p.feature.store.drop(_)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object StoreDefine extends ApplyFunction {
    val specification = FunctionSpecification("store-define", 2)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.argument1[Symbol]
        schema <- p.param.schema(p)
      } yield {
        (collection |@| schema)(p.feature.store.define(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  // private def _symbol_string(p: Parameters): (Symbol, String) = {
  //   p.argument(0)
  //   p.argument(1)
  //   ???
  // }
}
