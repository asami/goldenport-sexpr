package org.goldenport.sexpr

/**
 * @since   Sep.  9, 2012
 *  version Aug. 21, 2013
 *  version Jan.  9, 2014
 *  version Feb. 27, 2014
 *  version Apr. 23, 2014
 *  version May. 25, 2014
 * @version Aug.  4, 2014
 * @author  ASAMI, Tomoharu
 */
sealed trait SExpr {
  def toList: Option[List[SExpr]] = None
}

case class SAtom(name: String) extends SExpr {
  override def toString() = name
}

case class SKeyword(name: String) extends SExpr {
  override def toString() = ":" + name
}

case class SNumber(number: String) extends SExpr {
  override def toString() = number.toString
}

case class SString(string: String) extends SExpr {
  override def toString() = '"' + string + '"'
}

case class SBoolean(value: Boolean) extends SExpr {
  override def toString() = value.toString
}

object SBoolean {
  val TRUE = SBoolean(true)
  val FALSE = SBoolean(false)

  def isTrue(s: SExpr): Boolean = {
    s match {
      case SBoolean(false) => false
      case _ => true
    }
  }
}

sealed trait SList extends SExpr {
  def list: List[SExpr] = Nil
  override def toString() = list.mkString("(", " ", ")")
}

object SList {
  def apply(xs: SExpr*): SList = create(xs)

  def create(xs: Seq[SExpr]): SList = {
    xs.foldRight(SNil: SList) { (x, z) =>
      SCell(x, z)
    }
  }
}

case class SCell(car: SExpr, cdr: SExpr) extends SList {
  override def toList: Option[List[SExpr]] = Some(list)
  override def list: List[SExpr] = SExpr.build(this)
}

case object SNil extends SList

trait SPseudo extends SExpr

case object SOpen extends SPseudo // List Open
case object SClose extends SPseudo // List Close

object SExpr {
  def getKeyword[T](expr: SExpr, keyword: String)(implicit pf: PartialFunction[SExpr, T]): Option[T] = {
    expr.toList.flatMap(_.dropWhile(_ match {
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
      s.toList.get
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
      s.toList match {
        case Some(xs) => xs.collect {
          case s: SString => s.string
        }
        case _ => Nil
      }
    }
  }
}
