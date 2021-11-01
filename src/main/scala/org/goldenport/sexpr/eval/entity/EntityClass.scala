package org.goldenport.sexpr.eval.entity

import org.goldenport.context.Consequence
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.Field
import org.goldenport.statemachine.StateMachine

/*
 * @since   Sep. 18, 2021
 *  version Oct. 31, 2021
 * @version Nov.  1, 2021
 * @author  ASAMI, Tomoharu
 */
trait EntityClass {
  def create(p: IRecord): Consequence[Entity]
  def reconstitute(p: IRecord): Consequence[Entity]
  def unmarshallProperties(p: IRecord): Consequence[IRecord]
  def store: EntityClass.Store
  def idName: String = "id"
}

object EntityClass {
  trait Store {
    def idColumnName: String = "id" // TODO

    def idValue(p: EntityId): Any = p.objectId.string // TODO

    def toField(p: Field): Field = p // TODO

    def stateMachineToField(p: StateMachine): Field = ???

    def stateMachineToFieldValue(p: StateMachine): Field =
      Field.createInt(p.name, p.value)
  }
  object Store {
    class StringIdStore() extends Store {
    }

    def apply(): StringIdStore = new StringIdStore()
  }
}
