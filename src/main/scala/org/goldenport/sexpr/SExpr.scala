package org.goldenport.sexpr

/**
 * @since   Sep.  9, 2012
 * @version Sep.  9, 2012
 * @author  ASAMI, Tomoharu
 */
sealed trait SExpr {
  def list: Option[List[SExpr]] = None
}

case class SAtom(name: String) extends SExpr 

case class SKeyword(name: String) extends SExpr

case class SNumber(number: String) extends SExpr

case class SString(string: String) extends SExpr

sealed trait SList extends SExpr

case class SCell(car: SExpr, cdr: SExpr) extends SList {
  override def list: Option[List[SExpr]] = Some(SExpr.build(this))
}

case object SNil extends SList

object SExpr {
  def getKeyword[T](expr: SExpr, keyword: String)(implicit pf: PartialFunction[SExpr, T]): Option[T] = {
    expr.list.flatMap(_.dropWhile(_ match {
        case k: SKeyword if k.name == keyword => false
        case _ => true
      }) match {
        case x :: Nil => None
        case x :: xs => {
          pf.lift(xs.head)
        }
        case Nil => None
    })
  }

  def build(cell: SCell): List[SExpr] = {
    val buf = new scala.collection.mutable.ListBuffer[SExpr]
    var c = cell
    do { 
      buf += c.car
      c.cdr match {
        case SNil => c = null
        case x: SCell => c = x
        case _ => c = null
      }
    } while (c != null)
    buf.toList
  }

  implicit object SExprToSExpr extends PartialFunction[SExpr, SExpr] {
    def isDefinedAt(s: SExpr): Boolean = true
    def apply(s: SExpr): SExpr = s
  }

  implicit object SExprToString extends PartialFunction[SExpr, String] {
    def isDefinedAt(s: SExpr): Boolean = {
      s match {
        case _: SString => true
        case _ => false
      }
    }
    def apply(s: SExpr): String = {
      (s: @unchecked) match {
        case s: SString => s.string
      }
    }
  }

  implicit object SExprToSExprList extends PartialFunction[SExpr, List[SExpr]] {
    def isDefinedAt(s: SExpr): Boolean = {
      s match {
        case _: SCell => true
        case _ => false
      }
    }
    def apply(s: SExpr): List[SExpr] = {
      s.list.get
    }
  }

  implicit object SExprToStringList extends PartialFunction[SExpr, List[String]] {
    def isDefinedAt(s: SExpr): Boolean = {
      s match {
        case _: SCell => true
        case _ => false
      }
    }
    def apply(s: SExpr): List[String] = {
      s.list match {
        case Some(xs) => xs.collect {
          case s: SString => s.string
        }
        case _ => Nil
      }
    }
  }
}
