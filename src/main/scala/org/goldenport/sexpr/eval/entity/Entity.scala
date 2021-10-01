package org.goldenport.sexpr.eval.entity

/*
 * @since   Sep. 19, 2021
 * @version Sep. 24, 2021
 * @author  ASAMI, Tomoharu
 */
trait Entity {
  def id: EntityId
  def show: String
}
