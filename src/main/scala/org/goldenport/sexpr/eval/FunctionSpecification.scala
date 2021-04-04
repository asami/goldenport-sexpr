package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import org.goldenport.sexpr.SExpr
import org.goldenport.util.NumberUtils

/*
 * @since   Sep. 21, 2018
 *  version Mar. 10, 2019
 *  version Jul. 14, 2019
 * @version Mar. 21, 2021
 * @author  ASAMI, Tomoharu
 */
case class FunctionSpecification(
  name: String,
  numberOfMeaningfulParameters: Int = 0
) {
  def label(s: String): String = s"$name($numberOfMeaningfulParameters): $s"

  def label(e: Throwable): String = {
    val a = e match {
      case m: IllegalArgumentException => _message("Illegal Argument", m)
      case m => try {
        m.toString
      } catch {
        case NonFatal(e) => m.getClass.getSimpleName
      }
    }
    s"$name($numberOfMeaningfulParameters): $a"
  }

  private def _message(label: String, e: Throwable) = 
    Option(e.getMessage).map(x => s"$label: $x").getOrElse(label)

  def resolve(p: Parameters): Parameters = {
    case class Z(results: Vector[(Int, SExpr)] = Vector.empty) {
      def r = {
        val a = results.sortBy(_._1)
        case class Z1(xs: Vector[Option[SExpr]] = Vector.empty) {
          def r = {
            case class Z2(
              as: Vector[SExpr] = p.arguments.toVector,
              ys: Vector[SExpr] = Vector.empty) {
              def r = (ys ++ as).toList

              def +(rhs: Option[SExpr]) = rhs match {
                case Some(s) => copy(ys = ys :+ s)
                case None => as.headOption.
                    map(x => copy(as = as.tail, ys = ys :+ x)).
                    getOrElse(this)
              }
            }
            val z = xs./:(Z2())(_+_).r
            p.copy(arguments = z)
          }

          def +(rhs: (Int, SExpr)) = rhs match {
            case (n, x) =>
              val index = n - 1
              val b = if (xs.length < index)
                (xs ++ Vector.fill(index - xs.length)(None)) :+ Some(x)
              else if (xs.length > index)
                xs.updated(index, Some(x))
              else
                xs :+ Some(x)
              copy(xs = b)
          }
        }
        a./:(Z1())(_+_).r
      }

      def +(rhs: (Symbol, SExpr)) = rhs match {
        case (k, v) =>
          val regex = """arg(\d+)""".r // TODO
          regex.findFirstMatchIn(k.name) match {
            case Some(s) =>
              NumberUtils.getInt(s.group(1)).
                map(x => copy(results = results :+ (x, v))).
                getOrElse(this)
            case None => this
          }
      }
    }
    p.properties./:(Z())(_+_).r
  }
}

object FunctionSpecification {
  case class Signature()
  case class Parameters()
}
