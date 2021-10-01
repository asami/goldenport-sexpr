package org.goldenport.sexpr.eval.entity

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import org.goldenport.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.store.Query
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Mar. 30, 2019
 * @version Sep. 20, 2021
 * @author  ASAMI, Tomoharu
 */
object EntityFunction {
  val functions = Vector(
    EntityGet,
    EntityQuery,
    EntitySelect,
    EntityCreate,
    EntityUpdate,
    EntityDelete,
    EntityCreateCollection,
    EntityDeleteCollection
  )

  case object EntityGet extends IoFunction {
    val specification = FunctionSpecification("entity-get", 2)

    def apply(p: LispContext): LispContext = {
      // val db = _get_database(p.parameters)
      // val r = p.parameters.argument(1) match {
      // }
      val a = for {
        collection <- p.param.entityCollection
        id <- p.param.idForEntity
      } yield {
        (collection |@| id)(p.feature.entity.get(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntityQuery extends IoFunction {
    val specification = FunctionSpecification("entity-query", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.entityCollection
        query <- p.param.queryDefault
      } yield {
        (collection |@| query)(p.feature.entity.query(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntitySelect extends IoFunction {
    val specification = FunctionSpecification("entity-select", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.entityCollection
        query <- p.param.queryDefault
        header <- p.param.tableHeader(p)
      } yield {
        (collection |@| query |@| header)(p.feature.entity.select(_, _, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntityCreate extends IoFunction {
    val specification = FunctionSpecification("entity-create", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.entityCollection
        rec <- p.param.record
      } yield {
        (collection |@| rec)(p.feature.entity.create(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntityUpdate extends IoFunction {
    val specification = FunctionSpecification("entity-update", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.entityCollection
        id <- p.param.idForEntity
        rec <- p.param.record
      } yield {
        (collection |@| id |@| rec)(p.feature.entity.update(_, _, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntityDelete extends IoFunction {
    val specification = FunctionSpecification("entity-delete", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.entityCollection
        id <- p.param.idForEntity
      } yield {
        (collection |@| id)(p.feature.entity.delete(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntityCreateCollection extends IoFunction {
    val specification = FunctionSpecification("entity-create-collection", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.argument1[Symbol]
        entityclazz <- p.param.entityClass
      } yield {
        (collection |@| entityclazz)(p.feature.entity.createCollection(_, _)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  case object EntityDeleteCollection extends IoFunction {
    val specification = FunctionSpecification("entity-delete-collection", 1)

    def apply(p: LispContext): LispContext = {
      val a = for {
        collection <- p.param.entityCollection
      } yield {
        collection.map(p.feature.entity.deleteCollection(_)).valueOr(SError(_))
      }
      val r = a.run(p.param.cursor(specification))
      p.toResult(r._2)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  // private def _get_database(p: Parameters): Option[Symbol] = _get_property_symbol(p, 'database)

  // private def _symbol_string(p: Parameters): (Symbol, String) = {
  //   p.argument(0)
  //   p.argument(1)
  //   RAISE.notImplementedYetDefect
  // }

  // private def _get_property_symbol(p: Parameters, key: Symbol): Option[Symbol] = 
  //   p.getProperty(key).map {
  //     case SString(s) => Symbol(s)
  //     case SAtom(s) => Symbol(s)
  //     case m => RAISE.invalidArgumentFault(SError.invalidDatatype(key.name, m).print)
  //   }
}
