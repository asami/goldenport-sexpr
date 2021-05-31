package org.goldenport.sexpr.eval

import java.net.URI
import java.io.File
import breeze.linalg.{Vector => BVector, _}
// import breeze.plot._
import org.goldenport.RAISE
import org.goldenport.record.v2.bag.CsvBag
import org.goldenport.record.v3.Table
import org.goldenport.record.chart.{Chart, Space}
import org.goldenport.matrix.IMatrix
import org.goldenport.sexpr._
// import org.goldenport.sexpr.eval.chart.Chart

/*
 * @since   Feb.  9, 2019
 *  version Feb. 12, 2019
 *  version Mar. 10, 2019
 *  version Aug.  4, 2019
 *  version Sep. 19, 2019
 *  version Jan. 30, 2020
 * @version May. 20, 2021
 * @author  ASAMI, Tomoharu
 */
trait MatrixPart { self: LispFunction =>
  protected final def matrix_load(u: LispContext, p: URI): SExpr = {
    val h = u.takeResourceHandle(p)
    val strategy = csv_strategy(u.config)
    val csv = CsvBag.loadResource(h, strategy)
    val raw = csv.toMatrixString
    val t = Table.createStringAutoNumber(raw)
    val matrix = t.matrixDoubleDistilled
    SMatrix(matrix)
  }

  protected final def matrix_save(u: LispContext, uri: SUri, p: SExpr): SExpr =
    uri.getFile.
      map(matrix_save(u, _, _imatrix(p))).
      getOrElse(RAISE.notImplementedYetDefect)

  protected final def matrix_save(u: LispContext, file: File, p: SExpr): SExpr =
    matrix_save(u, file, _imatrix(p))

  protected final def matrix_save(u: LispContext, file: File, p: IMatrix[Double]): SExpr =
    SExpr.execute {
      val data = p.rowIterator.map(_.map(_.toString)).toVector
      val strategy = csv_strategy(u.config)
      val csv = CsvBag.create(file, strategy)
      csv.write(data) // TODO enable isForceDoubleQuote (a CSVWriter issue)
      SBoolean.TRUE
    }

  protected final def matrix_chart(u: LispContext, p: IMatrix[Double]): SExpr = {
    // val f = Figure()
    // val p = f.subplot(0)
    // val x = linspace(0.0,1.0)
    // p += plot(x, x :^ 2.0)
    // p += plot(x, x :^ 3.0, '.')
    // p.xlabel = "x axis"
    // p.ylabel = "y axis"
    // f.refresh()
    // SWindow(MatrixPart.BreezeFigureWindow(f))
    val plots = Vector.empty
    val series = SChartSeries("XY", plots)
    val chart = Chart.empty.
      withXLabel("x axis").
      withYLabel("y axis").
      withSeries(series.series)
    u.feature.chart.draw(chart)
  }

  private def _imatrix(p: SExpr): IMatrix[Double] = p match {
    case SMatrix(v) => v
    case m => RAISE.notImplementedYetDefect
  }
}

object MatrixPart {
  // case class CsvMatrix(bag: CsvBag) extends IMatrix[Double] {
  //   lazy val breeze: Matrix[Double] = {
  //     val m = bag.matrixDouble
  //     DenseMatrix.tabulate(m.height, m.width)((i, j) => m(i, j))
  //   }
  // }
  // object CsvMatrix {
  //   def create(uri: String): CsvMatrix = CsvMatrix(CsvBag.fromUri(uri))
  // }

  // case class BreezeMatrix(matrix: Matrix[Double]) extends IMatrix {
  // }
}
