package org.goldenport.sexpr

/*
 * @since   Sep. 18, 2014
 * @version Sep. 18, 2014
 * @author  ASAMI, Tomoharu
 */
object SExprConverters {
  def toString(x: SExpr): String = {
    x match {
      case SAtom(s) => s
      case SString(s) => s
      case x => throw new IllegalArgumentException(s"No atom or string = $x")
    }
  }

  def toStrings(xs: Seq[SExpr]): Seq[String] = {
    xs map toString
  }

  def toBoolean(xs: Seq[SExpr], name: String): Boolean = {
    val NAME = name
    xs.exists {
      case SAtom(NAME) => true
      case _ => false
    }
  }

  def toBooleanOpt(xs: Seq[SExpr], name: String): Option[Boolean] = {
    val NAME = name
    val a = xs collect {
      case SAtom(NAME) => Some(true)
    }
    if (a.contains(Some(true))) Some(true)
    else if (a.nonEmpty) Some(false)
    else None
  }

  def toBytes(xs: Seq[SExpr]): Seq[Byte] = {
    xs map {
      case SNumber(s) => s.toByte
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  def toShorts(xs: Seq[SExpr]): Seq[Short] = {
    xs map {
      case SNumber(s) => s.toShort
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  def toInts(xs: Seq[SExpr]): Seq[Int] = {
    xs map {
      case SNumber(s) => s.toInt
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  def toLongs(xs: Seq[SExpr]): Seq[Long] = {
    xs map {
      case SNumber(s) => s.toLong
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  def toFloats(xs: Seq[SExpr]): Seq[Float] = {
    xs map {
      case SNumber(s) => s.toFloat
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  def toDoubles(xs: Seq[SExpr]): Seq[Double] = {
    xs map {
      case SNumber(s) => s.toDouble
      case x => throw new IllegalArgumentException(s"No number = $x")
    }
  }

  def toValues[T](master: Map[String, T], xs: Seq[SExpr]): Seq[T] = {
    stringsToValues(master, toStrings(xs))
  }

  def stringsToValues[T](master: Map[String, T], xs: Seq[String]): Seq[T] = {
    xs map { x =>
      master.get(x) getOrElse {
        throw new IllegalArgumentException("Illegal symbol '$x', Available symbols are: ${master.keys.mkString(\", \")}")
      }
    }
  }

  def fromBoolean(v: Boolean) = {
    if (v) SBoolean.TRUE
    else SBoolean.FALSE
  }

  def fromInt(v: Int) = {
    SNumber(v.toString)
  }

  def fromLong(v: Long) = {
    SNumber(v.toString)
  }

  def fromFloat(v: Float) = {
    SNumber(v.toString)
  }

  def fromDouble(v: Double) = {
    SNumber(v.toString)
  }
}
