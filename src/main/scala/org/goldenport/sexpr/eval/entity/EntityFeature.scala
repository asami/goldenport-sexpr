package org.goldenport.sexpr.eval.entity

import org.goldenport.log.Loggable
import org.goldenport.hocon.RichConfig
import org.goldenport.i18n.I18NContext
import org.goldenport.value._
import org.goldenport.record.store.Query
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.store.StoreFeature

/*
 * @since   Sep. 18, 2021
 * @version Sep. 21, 2021
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

  def get(collection: EntityCollection, id: EntityId): SExpr = SExpr.execute(
    collection.get(id)
  )

  def query(collection: EntityCollection, q: Query): SExpr = SExpr.execute(
    collection.query(q)
  )

  def select(collection: EntityCollection, q: Query, header: Option[HeaderStrategy]): SExpr = SExpr.execute(
    collection.select(q, header)
  )

  def create(collection: EntityCollection, rec: Record): SExpr = SExpr.execute(
    collection.create(rec)
  )

  def update(entity: Entity): SExpr = SExpr.execute(
    ???
  )

  def update(collection: EntityCollection, id: EntityId, rec: Record): SExpr = SExpr.execute(
    collection.update(id, rec)
  )

  def delete(collection: EntityCollection, id: EntityId): SExpr = SExpr.execute(
    collection.delete(id)
  )

  def createCollection(name: Symbol, entityclass: EntityClass): SExpr = SExpr.execute(
    ???
  )

  def deleteCollection(collection: EntityCollection): SExpr = SExpr.execute(
    ???
  )

  def defineCollection(name: Symbol, entity: EntityClass): SExpr = SExpr.execute {
    factory.defineCollection(name, entity)
    SBoolean.TRUE
  }
}

object EntityFeature {
  def create(factory: EntityFactory) = new EntityFeature(RichConfig.empty, factory)
}
