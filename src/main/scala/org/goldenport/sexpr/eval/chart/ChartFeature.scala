package org.goldenport.sexpr.eval.chart

import org.goldenport.log.Loggable
import org.goldenport.sexpr._

/*
 * @since   Mar. 10, 2019
 * @version Mar. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class ChartFeature() extends Loggable {
  private val _driver = new JFreeChartDriver() // new BreezeBizChartDriver()

  def draw(p: S2DSpace): SExpr = SExpr.run(_driver.draw(p))
}

object ChartFeature {
  val default = ChartFeature()
}
