package org.goldenport.sexpr.eval.entity

/*
 * @since   Sep. 18, 2021
 * @version Sep. 20, 2021
 * @author  ASAMI, Tomoharu
 */
trait EntityFactory {
  def createId(id: String): EntityId
  def getCollection(collection: Symbol): Option[EntityCollection]
  def getCollection(store: Option[Symbol], collection: Symbol): Option[EntityCollection]

  def defineCollection(name: Symbol, entity: EntityClass): EntityCollection
}
