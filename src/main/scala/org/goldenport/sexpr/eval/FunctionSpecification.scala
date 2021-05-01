package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import org.goldenport.collection.VectorMap
import org.goldenport.record.v2.{XObject}
import org.goldenport.record.v3.Column
import org.goldenport.sexpr.SExpr
import org.goldenport.util.NumberUtils

/*
 * @since   Sep. 21, 2018
 *  version Mar. 10, 2019
 *  version Jul. 14, 2019
 *  version Mar. 21, 2021
 * @version Apr. 12, 2021
 * @author  ASAMI, Tomoharu
 */
case class FunctionSpecification(
  name: String,
  parameters: FunctionSpecification.Parameters
) {
  def numberOfRequiredArguments: Int = parameters.numberOfRequiredArguments

  def label(s: String): String = s"$name($numberOfRequiredArguments): $s"

  def label(e: Throwable): String = {
    val a = e match {
      case m: IllegalArgumentException => _message("Illegal Argument", m)
      case m => try {
        m.toString
      } catch {
        case NonFatal(e) => m.getClass.getSimpleName
      }
    }
    s"$name($numberOfRequiredArguments): $a"
  }

  private def _message(label: String, e: Throwable) = 
    Option(e.getMessage).map(x => s"$label: $x").getOrElse(label)

  def resolve(p: Parameters): Parameters = {
    case class Z(results: Vector[(Int, SExpr)] = Vector.empty) {
      def r: Parameters = {
        val a = results.sortBy(_._1)
        case class Z1(xs: Vector[Option[Parameters.Argument]] = Vector.empty) {
          def r: Parameters = {
            case class Z2(
              as: Vector[Parameters.Argument] = p.argumentVector,
              ys: Vector[Parameters.Argument] = Vector.empty
            ) {
              def r: Vector[Parameters.Argument] = ys ++ as

              def +(rhs: Option[Parameters.Argument]) = rhs match {
                case Some(s) => copy(ys = ys :+ s)
                case None => as.headOption.
                    map(x => copy(as = as.tail, ys = ys :+ x)).
                    getOrElse(this)
              }
            }
            val z = xs./:(Z2())(_+_).r
            val x = p.copy(argumentVector = z)
            x.resolve(FunctionSpecification.this)
          }

          def +(rhs: (Int, SExpr)) = rhs match {
            case (n, x0) =>
              val index = n - 1
              val paramname = parameters.makeParameterNameOneBased(index)
              val x = Parameters.Argument(paramname, x0)
              val b: Vector[Option[Parameters.Argument]] = if (xs.length < index)
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
          val regex = s"""${FunctionSpecification.Parameter.ARGUMENT_PREFIX}(\d+)""".r // TODO
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
  // case class Signature()

  case class Parameters(
    parameters: List[Parameter]
  ) {
    def numberOfRequiredArguments: Int = parameters.count(_.isArgument)

    def argumentNames: List[String] = parameters.withFilter(_.isArgument).map(_.name)

    def getParameterNameOneBased(p: Int): Option[String] = parameters.lift(p - 1).map(_.name)

    def makeParameterNameOneBased(p: Int): String =
      getParameterNameOneBased(p) getOrElse Parameter.argumentNameOneBased(p)
  }
  object Parameters {
    def apply(p: Parameter, ps: Parameter*): Parameters = Parameters((p +: ps).toList)
  }

  case class Parameter(
    metadata: Column,
    isArgument: Boolean
  ) extends Column.Holder {
    def column = metadata
  }
  object Parameter {
    val ARGUMENT_PREFIX = "arg"

    def apply(i: Int, isargument: Boolean): Parameter =
      apply(argumentNameOneBased(i), isargument)

    def apply(name: String, isargument: Boolean): Parameter =
      Parameter(Column(name, XObject), isargument)

    def argumentNameZeroBased(p: Int) = argumentNameOneBased(p + 1)
    def argumentNameOneBased(p: Int) = s"$ARGUMENT_PREFIX$p"
  }

  def apply(name: String): FunctionSpecification = apply(name, 0)

  def apply(name: String, numberofmeaningfulparameters: Int): FunctionSpecification = {
    val a = (1 to numberofmeaningfulparameters).map(Parameter(_, true))
    FunctionSpecification(name, Parameters(a.toList))
  }

  def apply(name: String, p: Parameter, ps: Parameter*): FunctionSpecification =
    FunctionSpecification(name, Parameters(p, ps: _*))
}
