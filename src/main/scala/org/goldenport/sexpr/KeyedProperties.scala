package org.goldenport.sexpr

import org.goldenport.sexpr._

/*
 * @since   Aug. 13, 2013
 * @version Aug. 21, 2013
 * @author  ASAMI, Tomoharu
 */
case class KeyedProperties(properties: Vector[KeyedProperty] = Vector.empty) {
  def :+(prop: (String, SExpr)): KeyedProperties = {
    copy(properties = properties :+ KeyedProperty(prop._1, prop._2))
  }

  def get(key: String): Option[SExpr] = {
    properties.find(_.key == key).map(_.value)
  }

  def asString(key: String): String = {
    getString(key) getOrElse {
      throw new IllegalArgumentException("Invalid key = " + key)
    }
  }

  def getString(key: String): Option[String] = {
    get(key).map(_.toString)
  }
}

case class KeyedProperty(key: String, value: SExpr) {
  
}
