package org.goldenport.sexpr.eval.entity

import org.goldenport.log.Loggable
import org.goldenport.hocon.RichConfig
import org.goldenport.i18n.I18NContext
import org.goldenport.value._
import org.goldenport.record.store.Query
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.LispContext
import org.goldenport.sexpr.eval.store.StoreFeature

/*
 * @since   Sep. 18, 2021
 *  version Sep. 21, 2021
 *  version Oct.  2, 2021
 * @version Nov. 28, 2021
 * @author  ASAMI, Tomoharu
 */
class EntityFeature(
  val config: RichConfig,
  val factory: EntityFactory
) extends Loggable {
  import EntityFeature._

  def createId(id: String): EntityId = factory.createId(id)

  def getCollection(collection: Symbol): Option[EntityCollection] = factory.getCollection(collection)

  def getCollection(store: Option[Symbol], collection: Symbol): Option[EntityCollection] =
    factory.getCollection(store, collection)

  def get(id: EntityId): SExpr = SExpr.execute(
    getCollection(Symbol(id.className)).map(_.get(id.objectId)).getOrElse(SError.notFound(id.string))
  )

  def get(collection: EntityCollection, id: EntityId): SExpr = SExpr.execute(
    collection.get(id.objectId)
  )

  def query(collection: EntityCollection, q: Query): SExpr = SExpr.execute(
    collection.query(q)
  )

  def select(collection: EntityCollection, q: Query, header: Option[HeaderStrategy]): SExpr = SExpr.execute(
    collection.select(q, header)
  )

  def create(
    collection: EntityCollection,
    rec: Record
  )(implicit ctx: LispContext): SExpr = SExpr.execute(
    collection.create(rec)
  )

  def update(
    entity: Entity
  )(implicit ctx: LispContext): SExpr = SExpr.execute {
    factory.getCollection(entity.id.className).
      map(update(_, entity.id, entity.persistentRecord)).
      getOrElse(SError.notFound(entity.id.className))
  }

  def update(
    collection: EntityCollection,
    id: EntityId,
    rec: IRecord
  )(implicit ctx: LispContext): SExpr = SExpr.execute(
    collection.update(id.objectId, rec)
  )

  def delete(
    collection: EntityCollection,
    id: EntityId
  )(implicit ctx: LispContext): SExpr = SExpr.execute(
    collection.delete(id.objectId)
  )

  def createCollection(collection: EntityCollection): SExpr = SExpr.execute(
    collection.constitute()
  )

  def createCollection(name: Symbol, entityclass: EntityClass): SExpr = SExpr.execute(
    ???
  )

  def deleteCollection(collection: EntityCollection): SExpr = SExpr.execute(
    collection.destroy()
  )

  def defineCollection(name: Symbol, entity: EntityClass): SExpr = SExpr.execute {
    factory.defineCollection(name, entity)
    SBoolean.TRUE
  }
}

object EntityFeature {
  def create(factory: EntityFactory) = new EntityFeature(RichConfig.empty, factory)
}
