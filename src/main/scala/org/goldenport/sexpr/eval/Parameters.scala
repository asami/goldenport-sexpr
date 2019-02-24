package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import org.goldenport.sexpr._
import org.goldenport.sexpr.SExprConverter._

/*
 * @since   Sep. 25, 2018
 * @version Oct. 28, 2018
 * @author  ASAMI, Tomoharu
 */
case class Parameters(
  arguments: List[SExpr],
  properties: Map[Symbol, SExpr],
  switches: Set[Symbol]
) {
  def show = "Paramerters()" // TODO

  def arguments1[A](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A]) = a.apply(arguments(0))
  def arguments2[A, B](
    spec: FunctionSpecification
  )(implicit a: SExprConverter[A], b: SExprConverter[B]): (A, B) = ???
  def arguments3[A, B, C](spec: FunctionSpecification): (A, B, C) = ???

  def asStringList: List[String] = arguments.map(_.asString)
  def asBigDecimalList: List[BigDecimal] = arguments.map(_.asBigDecimal)

  def getProperty(p: Symbol): Option[SExpr] = properties.get(p)
  def isSwitch(p: Symbol): Boolean = switches.contains(p)
}
object Parameters {
  def apply(ps: List[SExpr]): Parameters = {
    case class Z(
      as: Vector[SExpr] = Vector.empty,
      props: Map[Symbol, Vector[SExpr]] = Map.empty,
      switches: Set[Symbol] = Set.empty,
      keyword: Option[Symbol] = None
    ) {
      def r = {
        val ps: Map[Symbol, SExpr] = props.mapValues(xs => xs.length match {
          case 0 => SNil
          case 1 => xs(0)
          case _ => SList.create(xs)
        })
        val ss = keyword.fold(switches)(switches + _)
        Parameters(as.toList, ps, ss)
      }
      def +(rhs: SExpr) = keyword.map(k =>
        rhs match {
          case m: SKeyword => copy(switches = switches + Symbol(m.name), keyword = None)
          case SNil => copy(switches = switches + Symbol(k.name), keyword = None)
          case m => copy(props = props |+| Map(Symbol(k.name) -> Vector(m)))
        }
      ).getOrElse(
        rhs match {
          case m: SKeyword => copy(keyword = Some(Symbol(m.name)))
          case m => copy(as = as :+ m)
        }
      )
    }
    ps./:(Z())(_+_).r
  }
}
