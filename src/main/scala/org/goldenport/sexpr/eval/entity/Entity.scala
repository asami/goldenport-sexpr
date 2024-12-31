package org.goldenport.sexpr.eval.entity

import org.goldenport.collection.VectorMap
import org.goldenport.record.v3.IRecord
import org.goldenport.sm.StateMachine

/*
 * @since   Sep. 19, 2021
 *  version Sep. 24, 2021
 *  version Oct. 23, 2021
 *  version Nov. 21, 2021
 *  version Feb. 28, 2023
 * @version Sep.  6, 2024
 * @author  ASAMI, Tomoharu
 */
trait Entity {
  def id: EntityId
  def attributes: IRecord
  def statemachines: VectorMap[Symbol, StateMachine]
  def show: String
  def record: IRecord
  def recordWithShortId: IRecord
  def persistentRecord: IRecord
}
