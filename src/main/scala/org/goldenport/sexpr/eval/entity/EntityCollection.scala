package org.goldenport.sexpr.eval.entity

import org.goldenport.record.v3.Record
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.record.store.Query
import org.goldenport.sexpr._

/*
 * @since   Sep. 18, 2021
 * @version Sep. 21, 2021
 * @author  ASAMI, Tomoharu
 */
trait EntityCollection {
  def get(id: EntityId): SExpr
  def query(q: Query): SExpr
  def select(q: Query, header: Option[HeaderStrategy]): SExpr
  def create(rec: Record): SExpr
  def update(id: EntityId, rec: Record): SExpr
  def delete(id: EntityId): SExpr
}

object EntityCollection {
}
