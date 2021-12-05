package org.goldenport.sexpr.eval.entity

import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.record.store.Id
import org.goldenport.record.store.Query
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.LispContext

/*
 * @since   Sep. 18, 2021
 *  version Sep. 21, 2021
 *  version Oct.  2, 2021
 * @version Nov. 28, 2021
 * @author  ASAMI, Tomoharu
 */
trait EntityCollection {
  def get(id: Id): SExpr
  def query(q: Query): SExpr
  def select(q: Query, header: Option[HeaderStrategy]): SExpr
  def create(rec: IRecord)(implicit ctx: LispContext): SExpr
  def update(id: Id, rec: IRecord)(implicit ctx: LispContext): SExpr
  def delete(id: Id)(implicit ctx: LispContext): SExpr
  def constitute(): SExpr
  def destroy(): SExpr
}

object EntityCollection {
}
