package org.goldenport.sexpr.eval.chart

import org.goldenport.log.Loggable
import org.goldenport.record.v3.ITable
import org.goldenport.record.chart._
import org.goldenport.record.chart.driver.JFreeChartDriver
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.LispContext

/*
 * @since   Mar. 10, 2019
 *  version Sep. 19, 2019
 *  version Feb. 29, 2020
 * @version May. 20, 2021
 * @author  ASAMI, Tomoharu
 */
case class ChartFeature() extends Loggable {
  private val _driver = new JFreeChartDriver() // new BreezeBizChartDriver()

  // def buildChart(u: LispContext, p: ITable, analyze: List[Symbol]): Chart = {
  //   // val space = buildSpace(u, p)
  //   val space = buildSpaceWithSimpleRegression(u, p)
  //   Chart.Builder(u.i18nContext)(space)
  // }

  // def buildSpace(u: LispContext, p: ITable): Space = {
  //   val series = buildSeries(u, p)
  //   Space(series)
  // }

  // def buildSpaceWithSimpleRegression(u: LispContext, p: ITable): Space = {
  //   val b = _series_builder(u, p)
  //   val main = b(p)
  //   val regression = b.simpleRegression(p)
  //   Space(main, regression)
  // }

  // def buildSeries(u: LispContext, p: ITable): Series =  _series_builder(u, p)(p)

  def buildChart(u: LispContext, p: ITable, analyzes: List[String]): Chart = {
    val sr = _series_builder(u, p)
    val scatter = sr.apply(p) // TODO
    val xs = analyzes.map {
      case "simple-regression" => sr.simpleRegression(p)
      case m => SError.invalidArgument("analyze", m).RAISE
    }
    val series = scatter +: xs
    val space = Space(series)
    Chart.Builder(u.i18nContext)(space)
  }

  private def _series_builder(u: LispContext, p: ITable): Series.Builder = {
    val schema = p.schema
    val xc = schema.columns(1) // XXX
    val yc = schema.columns(2) // XXX
    val tc = Some(schema.columns(0)) // XXX
    Series.Builder(u.i18nContext, u.numericalOperations, xc, yc, tc)
  }

  def draw(p: Chart): SExpr = SExpr.execute(SWindow(_driver.draw(p).window))
}

object ChartFeature {
  val default = ChartFeature()
}
