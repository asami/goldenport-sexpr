package org.goldenport.sexpr.eval.chart

import org.goldenport.log.Loggable
import org.goldenport.record.v3.ITable
import org.goldenport.record.chart._
import org.goldenport.record.chart.driver.JFreeChartDriver
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.LispContext

/*
 * @since   Mar. 10, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
case class ChartFeature() extends Loggable {
  private val _driver = new JFreeChartDriver() // new BreezeBizChartDriver()

  def buildChart(u: LispContext, p: ITable): Chart = {
    val space = buildSpace(u, p)
    Chart.Builder(u.i18nContext)(space)
  }

  def buildSpace(u: LispContext, p: ITable): Space = {
    val series = buildSeries(u, p)
    Space(series)
  }

  def buildSeries(u: LispContext, p: ITable): Series = {
    val schema = p.schema
    val xc = schema.columns(1) // XXX
    val yc = schema.columns(2) // XXX
    val tc = Some(schema.columns(0)) // XXX
    Series.Builder(u.i18nContext, xc, yc, tc)(p)
  }

  def draw(p: Chart): SExpr = SExpr.run(SWindow(_driver.draw(p).window))
}

object ChartFeature {
  val default = ChartFeature()
}
