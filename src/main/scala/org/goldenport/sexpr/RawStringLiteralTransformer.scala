package org.goldenport.sexpr

import com.asamioffice.goldenport.text.UJavaString

/*
 * @since   Sep. 14, 2014
 * @version Sep. 14, 2014
 * @author  ASAMI, Tomoharu
 */
object RawStringLiteralTransformer {
  def transform(s: String): String = {
    val a = s.split("\"\"\"")
    if (a.length > 1) {
      val b = _transform(a)
      b.mkString
    } else {
      s
    }
  }

  private def _transform(xs: Seq[String]): Seq[String] = {
    case class Z(
      state: State = ContentState,
      z: Vector[String] = Vector.empty
    ) {
      def action(s: String): Z = {
        val (next, result) = state.action(s)
        Z(next, z ++ result)
      }
    }
    def f(z: Z, s: String): Z = {
      z.action(s)
    }
    xs.foldLeft(Z())(f).z
  }

  sealed trait State {
    def action(s: String): (State, Vector[String])
  }

  case object ContentState extends State {
    def action(s: String): (State, Vector[String]) = {
      val r = s
      (RawState, Vector(r))
    }
  }

  case object RawState extends State {
    def action(s: String): (State, Vector[String]) = {
      val a = UJavaString.escapeJavaText(s)
      val r = "\"" + a + "\""
      (ContentState, Vector(r))
    }
  }
}
