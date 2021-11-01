package org.goldenport.sexpr.eval.entity

import org.goldenport.record.v3.IRecord

/*
 * @since   Sep. 19, 2021
 *  version Sep. 24, 2021
 *  version Oct. 23, 2021
 * @version Nov.  1, 2021
 * @author  ASAMI, Tomoharu
 */
trait Entity {
  def id: EntityId
  def show: String
  def record: IRecord
  def persistentRecord: IRecord
}
