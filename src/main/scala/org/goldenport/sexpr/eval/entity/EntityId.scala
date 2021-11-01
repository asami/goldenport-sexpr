package org.goldenport.sexpr.eval.entity

import org.goldenport.Strings
import org.goldenport.extension.Showable
import org.goldenport.context.Conclusion
import org.goldenport.record.store.Id
import org.goldenport.values.CompactUuid

/*
 * @since   Sep. 18, 2021
 *  version Sep. 24, 2021
 *  version Oct. 30, 2021
 * @version Nov.  1, 2021
 * @author  ASAMI, Tomoharu
 */
case class EntityId(
  className: String,
  objectId: Id
) extends Showable {
  def string = s"${className}-${objectId.string}"
  def print = string
  def display = string
  def show = string
  def embed = string
}

object EntityId {
  def apply(classname: String, id: String): EntityId = EntityId(classname, Id(id))

  def apply(classname: String, id: Long): EntityId = EntityId(classname, Id(id))

  def create(p: String): EntityId = Strings.totokens(p, "-") match {
    case Nil => Conclusion.invalidTokenFault(p).RAISE
    case x :: Nil => apply("", x)
    case x :: xs => apply(x, xs.mkString("-"))
  }

  def generate(classname: String): EntityId = EntityId(classname, Id(CompactUuid.generateString()))
}
// trait EntityId extends Showable {
//   def className: String
//   def objectId: Id
//   def string: String
//   def value: Any
// }

// object EntityId {
//   case class StringEntityId(className: String, id: String) extends EntityId {
//     def value = id
//     def string = id
//     def show = string
//   }

//   case class LongEntityId(className: String, id: Long) extends EntityId {
//     def value = id
//     def string = id.toString
//     def show = string
//   }

//   def apply(classname: String, id: String): StringEntityId = StringEntityId(classname, id)

//   def apply(classname: String, id: Long): LongEntityId = LongEntityId(classname, id)

//   def generate(classname: String): StringEntityId = StringEntityId(classname, CompactUuid.generateString())
// }
