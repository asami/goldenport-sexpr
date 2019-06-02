package org.goldenport.sexpr.eval.chart

import org.goldenport.log.Loggable
import org.goldenport.sexpr._

/*
 * @since   Mar. 10, 2019
 * @version Mar. 10, 2019
 * @author  ASAMI, Tomoharu
 */
trait ChartDriver extends Loggable {
  def draw(p: S2DSpace): SWindow
}
