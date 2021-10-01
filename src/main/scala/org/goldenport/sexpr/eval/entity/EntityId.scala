package org.goldenport.sexpr.eval.entity

import org.goldenport.values.CompactUuid

/*
 * @since   Sep. 18, 2021
 * @version Sep. 24, 2021
 * @author  ASAMI, Tomoharu
 */
trait EntityId {
  def text: String
}

object EntityId {
  case class StringEntityId(id: String) extends EntityId {
    def text = id
  }

  def create(id: String): StringEntityId = StringEntityId(id)

  def generate(): StringEntityId = StringEntityId(CompactUuid.generateString())
}
