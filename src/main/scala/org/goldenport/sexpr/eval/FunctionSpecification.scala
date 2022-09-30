package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import org.goldenport.collection.VectorMap
import org.goldenport.record.v2.{DataType, XObject}
import org.goldenport.record.v2.MZeroOne
import org.goldenport.record.v3.Column
import org.goldenport.sexpr.SExpr
import org.goldenport.util.NumberUtils

/*
 * @since   Sep. 21, 2018
 *  version Mar. 10, 2019
 *  version Jul. 14, 2019
 *  version Mar. 21, 2021
 *  version Apr. 12, 2021
 *  version Jun. 26, 2021
 *  version Oct. 31, 2021
 *  version Aug. 31, 2022
 * @version Sep.  1, 2022
 * @author  ASAMI, Tomoharu
 */
case class FunctionSpecification(
  name: String,
  signature: FunctionSpecification.Signature
) {
  def numberOfRequiredArguments: Int = signature.numberOfRequiredArguments

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
            x.resolve(FunctionSpecification.this).take // TODO
          }

          def +(rhs: (Int, SExpr)) = rhs match {
            case (n, x0) =>
              val index = n // n - 1
              val paramname = signature.makeParameterNameOneBased(index)
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
          val regex = (FunctionSpecification.Parameter.ARGUMENT_PREFIX + """(\d+)""").r
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
  case class Signature(
    parameters: Parameters
  ) {
    def numberOfRequiredArguments: Int = parameters.numberOfRequiredArguments
    def makeParameterNameOneBased(p: Int): String = parameters.makeParameterNameOneBased(p)
  }

  case class Parameters(
    arguments: List[Parameter.Argument],
    variableArityArgument: Option[Parameter.Argument],
    properties: List[Parameter.Property],
    switches: List[Parameter.Switch]
  ) {
    def numberOfRequiredArguments: Int = arguments.count(_.isRequiredArgument)

    def argumentNames: List[String] = arguments.withFilter(_.isArgument).map(_.name)

    def getParameterNameOneBased(p: Int): Option[String] = arguments.lift(p - 1).map(_.name)

    def makeParameterNameOneBased(p: Int): String =
      getParameterNameOneBased(p) getOrElse Parameter.argumentNameOneBased(p)
  }
  object Parameters {
    def apply(p: Parameter, ps: Parameter*): Parameters = Parameters((p +: ps).toList)

    def apply(ps: Seq[Parameter]): Parameters = apply(ps, None)

    def apply(ps: Seq[Parameter], vaa: Parameter.Argument): Parameters = apply(ps, Some(vaa))

    def apply(ps: Seq[Parameter], vaa: Option[Parameter.Argument]): Parameters = {
      val a = ps.collect {
        case m: Parameter.Argument => m
      }
      val b = ps.collect {
        case m: Parameter.Property => m
      }
      val c = ps.collect {
        case m: Parameter.Switch => m
      }
      Parameters(a.toList, vaa, b.toList, c.toList)
    }
  }

  sealed trait Parameter extends Column.Holder {
    def metadata: Column
    def isArgument: Boolean
    def isRequired = metadata.isRequired
    def isRequiredArgument = isArgument && isRequired
    def column = metadata
  }
  object Parameter {
    case class Argument(
      metadata: Column
    ) extends Parameter {
      val isArgument: Boolean = true
    }
    object Argument {
      def apply(name: String, datatype: DataType): Argument = Argument(Column(name, datatype))
    }

    case class Property(
      metadata: Column
    ) extends Parameter {
      val isArgument: Boolean = false
    }

    case class Switch(
      metadata: Column
    ) extends Parameter {
      val isArgument: Boolean = false
    }

    val ARGUMENT_PREFIX = "arg"
    val VARIABLE_ARITY_ARGUMENT_NAME = "argN"

    def argument(i: Int): Parameter = argument(argumentNameOneBased(i))

    def argument(i: Int, datatype: DataType): Parameter = argument(argumentNameOneBased(i), datatype)

    def argument(name: String): Parameter = argument(name, XObject)

    def argument(name: String, datatype: DataType): Parameter = Parameter.Argument(Column(name, datatype))

    def argumentNameZeroBased(p: Int) = argumentNameOneBased(p + 1)
    def argumentNameOneBased(p: Int) = s"$ARGUMENT_PREFIX$p"

    def argumentOption(name: String): Parameter =
      Parameter.Argument(Column(name, XObject, MZeroOne))

    def argumentOption(name: String, dt: DataType): Parameter =
      Parameter.Argument(Column(name, dt, MZeroOne))

    def propertyOption(name: String): Parameter =
      Parameter.Property(Column(name, XObject, MZeroOne))

    def propertyOption(name: String, dt: DataType): Parameter =
      Parameter.Property(Column(name, dt, MZeroOne))
  }

  def apply(name: String, p: Parameter, ps: Parameter*): FunctionSpecification =
    FunctionSpecification(name,Signature(Parameters(p, ps: _*)))

  def apply(name: String): FunctionSpecification = apply(name, 0)

  def apply(name: String, numberofmeaningfulparameters: Int): FunctionSpecification = {
    val a = (1 to numberofmeaningfulparameters).map(Parameter.argument)
    FunctionSpecification(name, Signature(Parameters(a.toList)))
  }

  def variableArityArgument(
    name: String,
    numberofmeaningfulparameters: Int,
    datatype: DataType
  ): FunctionSpecification = {
    val a = (1 to numberofmeaningfulparameters).map(Parameter.argument(_, datatype))
    val b = Parameter.Argument(Parameter.VARIABLE_ARITY_ARGUMENT_NAME, datatype)
    FunctionSpecification(name, Signature(Parameters(a.toList, b)))
  }
}
