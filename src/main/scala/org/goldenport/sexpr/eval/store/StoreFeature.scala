package org.goldenport.sexpr.eval.store

import org.goldenport.log.Loggable
import org.goldenport.hocon.RichConfig
import org.goldenport.record.store._
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.sexpr._

/*
 * @since   Mar. 30, 2019
 *  version Apr. 15, 2019
 * @version May.  9, 2019
 * @author  ASAMI, Tomoharu
 */
class StoreFeature(val config: RichConfig, val sqlContext: SqlContext) extends Loggable {
  val factory = new StoreFactory(config, sqlContext)

  def getCollection(store: Option[Symbol], collection: Symbol): Option[Collection] =
    factory.getCollection(store, collection)

  def get(collection: Collection, id: Id): SExpr = SExpr.run(
    collection.get(id).map(SRecord).getOrElse(SError.notFound(s"Store[${collection.name}]", id.show))
  )

  def query(collection: Collection, q: Query): SExpr = SExpr.run(
    SList.create(collection.query(q).vector.map(SRecord))
  )

  def insert(collection: Collection, rec: Record): SExpr = SExpr.run {
    val rs = to_store_records(rec)
    SString(collection.insert(rs).string)
  }

  def update(collection: Collection, id: Id, rec: Record): SExpr = SExpr.run {
    val rs = to_store_records(rec)
    collection.update(id, rs)
    SBoolean.TRUE
  }

  protected final def to_store_records(p: Record): Record = p.mapValue {
    case m: SExpr => m.asObject
    case m => m
  }

  def delete(collection: Collection, id: Id): SExpr = SExpr.run {
    collection.delete(id)
    SBoolean.TRUE
  }

  def create(store: Option[Symbol], name: Symbol, schema: Schema): SExpr = SExpr.run {
    factory.createCollection(store, name, schema)
    SBoolean.TRUE
  }

  def create(name: Symbol, schema: Schema): SExpr = SExpr.run {
    factory.createCollection(name, schema)
    SBoolean.TRUE
  }

  def create(store: Symbol, name: Symbol, schema: Schema): SExpr = SExpr.run {
    factory.createCollection(store, name, schema)
    SBoolean.TRUE
  }

  def drop(collection: Collection): SExpr = SExpr.run {
    collection.drop()
    SBoolean.TRUE
  }
}

object StoreFeature {
  val empty = new StoreFeature(RichConfig.empty, SqlContext.empty)
}
