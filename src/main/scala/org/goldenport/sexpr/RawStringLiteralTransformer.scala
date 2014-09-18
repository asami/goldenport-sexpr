package org.goldenport.sexpr

import com.asamioffice.goldenport.text.UJavaString

/*
 * @since   Sep. 14, 2014
 * @version Sep. 18, 2014
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

// object RawStringLiteralTransformer2 {
//   import scalaz._, Scalaz._

//   type TransformState = State[St, Seq[String]]

//   def transform(s: String): String = {
//     val a = s.split("\"\"\"")
//     if (a.length > 1) {
//       val b = _transform(a)
//       b.mkString
//     } else {
//       s
//     }
//   }

//   private def _transform(xs: Seq[String]): Seq[String] = {
//     val a: TransformState = State(s1 => (s1, s1.strings))
//     val c = xs.foldLeft(a)((z, x) => z.flatMap(_add(x)))
//     c
//   }

//   private def _add(x: String)(xs: Seq[String]): TransformState = {
//     State(_.action(x))
//   }

//   sealed trait St {
//     def strings: Vector[String]
//     def action(s: String): (St, Vector[String])
//   }

//   case class ContentState(strings: Vector[String]) extends St {
//     def action(s: String): (St, Vector[String]) = {
//       val r = s
//       val xs = strings :+ r
//       (RawState(xs), xs)
//     }
//   }

//   case class RawState(strings: Vector[String]) extends St {
//     def action(s: String): (St, Vector[String]) = {
//       val a = UJavaString.escapeJavaText(s)
//       val r = "\"" + a + "\""
//       val xs = strings :+ r
//       (ContentState(xs), xs)
//     }
//   }
// }
