package org.goldenport.sexpr.eval.chart

import java.awt.Color
import breeze.linalg._
// import breeze.plot._
import org.goldenport.RAISE
import org.goldenport.log.Loggable
import org.goldenport.extension.IWindow
import org.goldenport.sexpr._

/*
 * @since   Mar. 10, 2019
 * @version Sep. 19, 2019
 * @author  ASAMI, Tomoharu
 */
// class BreezeBizChartDriver() extends ChartDriver {
//   val name = "breeze-biz"

//   def draw(p: S2DSpace): SWindow = {
//     val f = Figure()
//     val plot = f.subplot(0)
//     // val x = linspace(0.0,1.0)
//     // p += plot(x, x :^ 2.0)
//     // p += plot(x, x :^ 3.0, '.')
//     // p.xlabel = "x axis"
//     // p.ylabel = "y axis"
//     val x = DenseVector(1.0)
//     val y = DenseVector(1.0)
//     val size = Map(0 -> 10.0)
//     val color = Map(0 -> Color.RED)
//     val label = Map(0 -> "ラベル")
//     val tips = Map(0 -> "Tips")
//     plot += scatter(x, y, size, color, labels = label, tips = tips)
//     p.chart.flatMap(_.xLabel).foreach(x => plot.xlabel = x)
//     p.chart.flatMap(_.yLabel).foreach(x => plot.ylabel = x)
//     f.refresh()
//     SWindow(BreezeBizChartDriver.BreezeFigureWindow(f))
//   }
// }

// object BreezeBizChartDriver {
//   case class BreezeFigureWindow(figure: Figure) extends IWindow {
//     def close() = RAISE.notImplementedYetDefect
//   }
// }
