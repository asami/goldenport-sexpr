package org.goldenport.sexpr.eval.store

import org.goldenport.log.Loggable
import org.goldenport.hocon.RichConfig
import org.goldenport.i18n.I18NContext
import org.goldenport.value._
import org.goldenport.record.store._
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.sexpr._

/*
 * @since   Mar. 30, 2019
 *  version Apr. 15, 2019
 *  version May.  9, 2019
 *  version Jul. 21, 2019
 *  version Oct.  7, 2019
 *  version Nov. 27, 2019
 *  version Mar. 30, 2020
 * @version Feb. 26, 2021
 * @author  ASAMI, Tomoharu
 */
class StoreFeature(
  val config: RichConfig,
  val i18nContext: I18NContext,
  val sqlContext: SqlContext
) extends Loggable {
  import StoreFeature._

  val factory = new StoreFactory(config, sqlContext)

  def getCollection(collection: Symbol): Option[Collection] = factory.getCollection(collection)

  def getCollection(store: Option[Symbol], collection: Symbol): Option[Collection] =
    factory.getCollection(store, collection)

  def get(collection: Collection, id: Id): SExpr = SExpr.run(
    collection.get(id).map(SRecord.apply).getOrElse(SError.notFound(s"Store[${collection.name}]", id.show))
  )

  def select(collection: Collection, q: Query, header: Option[HeaderStrategy]): SExpr = SExpr.run {
    val a = collection.select(q)
    val t = header.map(x => a.toTable(i18nContext, x)).getOrElse(a.toTable)
    STable(t)
  }

  def insert(collection: Collection, rec: Record): SExpr = SExpr.run {
    val r = to_store_record(rec)
    SString(collection.insert(r).string)
  }

  def inserts(collection: Collection, ps: STable): SExpr = inserts(collection, ps.toRecordSequence)

  def inserts(collection: Collection, ps: Seq[Record]): SExpr = SExpr.run {
    val rs = to_store_records(ps)
    val r = collection.inserts(rs)
    SList.create(r.map(x => SString(x.string)))
  }

  def inserts(collection: Collection, ps: RecordSequence): SExpr = SExpr.run {
    val rs = to_store_records(ps)
    val r = collection.inserts(rs)
    SList.create(r.map(x => SString(x.string)))
  }

  def update(collection: Collection, id: Id, rec: Record): SExpr = SExpr.run {
    val r = to_store_record(rec)
    collection.update(id, r)
    SBoolean.TRUE
  }

  protected final def to_store_records(ps: Seq[Record]): Seq[Record] =
    ps.map(to_store_record)

  protected final def to_store_records(p: RecordSequence): Seq[Record] =
    p.toRecords.map(to_store_record)

  protected final def to_store_record(p: Record): Record = p.mapValue {
    case m: SExpr => m.asObject
    case m => m
  }

  def delete(collection: Collection, id: Id): SExpr = SExpr.run {
    collection.delete(id)
    SBoolean.TRUE
  }

  def create(collection: Collection): SExpr = SExpr.run {
    collection.create
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

  def define(store: Option[Symbol], name: Symbol, schema: Schema): SExpr = SExpr.run {
    factory.defineCollection(store, name, schema)
    SBoolean.TRUE
  }

  def define(name: Symbol, schema: Schema): SExpr = SExpr.run {
    factory.defineCollection(name, schema)
    SBoolean.TRUE
  }

  def define(store: Symbol, name: Symbol, schema: Schema): SExpr = SExpr.run {
    factory.defineCollection(store, name, schema)
    SBoolean.TRUE
  }

  def imports(collection: Symbol, data: STable): SExpr = SExpr.run {
    getCollection(collection).
      map(c => inserts(c, data)). // TODO more generic
      getOrElse(SError.notImplementedYetDefect("SoreFeature#imports"))
  }

  def setup(collection: Symbol, data: STable): SExpr = SExpr.run {
    getCollection(collection).
      map { c => // TODO more generic
        inserts(c, data) match {
          case m: SError => // TODO
            create(c)
            inserts(c, data)
          case m => m
        }
      }.getOrElse(SError.notImplementedYetDefect("SoreFeature#imports"))
  }
}

object StoreFeature {
  def now() = new StoreFeature(RichConfig.empty, I18NContext.default, SqlContext.now())
}
