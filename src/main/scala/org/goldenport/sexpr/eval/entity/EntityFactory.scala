package org.goldenport.sexpr.eval.entity

import org.goldenport.RAISE

/*
 * @since   Sep. 18, 2021
 *  version Sep. 20, 2021
 *  version Nov. 19, 2021
 * @version Sep.  7, 2024
 * @author  ASAMI, Tomoharu
 */
trait EntityFactory {
  def createId(id: String): EntityId
  def getCollection(collection: Symbol): Option[EntityCollection]
  def getCollection(collection: String): Option[EntityCollection] = getCollection(Symbol(collection))
  def getCollection(store: Option[Symbol], collection: Symbol): Option[EntityCollection]

  def defineCollection(name: Symbol, entity: EntityClass): EntityCollection
}

object EntityFactory {
  val empty = new EntityFactory {
    def createId(id: String): EntityId = RAISE.unsupportedOperationFault
    def getCollection(collection: Symbol): Option[EntityCollection] = None
    def getCollection(store: Option[Symbol], collection: Symbol): Option[EntityCollection] = None

    def defineCollection(name: Symbol, entity: EntityClass): EntityCollection =
      RAISE.unsupportedOperationFault
  }
}
