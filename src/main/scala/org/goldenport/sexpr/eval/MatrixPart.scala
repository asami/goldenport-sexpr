package org.goldenport.sexpr.eval

import java.net.URI
import breeze.linalg._
import breeze.plot._
import org.goldenport.record.v2.bag.CsvBag
import org.goldenport.sexpr._
import org.goldenport.matrix.IMatrix

/*
 * @since   Feb.  9, 2019
 * @version Feb. 12, 2019
 * @author  ASAMI, Tomoharu
 */
trait MatrixPart { self: LispFunction =>
  protected final def matrix_load(u: LispContext, p: URI): SExpr = {
    val config = u.config
    val strategy = CsvBag.Strategy.matrixAuto.update(
      Some(CsvBag.Strategy.matrixAuto.recordBagStrategy.update(
        config.getString("csv.codec").map(scalax.io.Codec.apply),
        None,
        None,
        None
      )),
      config.getString("csv.name"),
      config.getString("csv.lineEnd"),
      config.getBoolean("csv.isForceDoubleQuote")
    )
    val csv = CsvBag.load(p, strategy)
    val matrix = csv.toMatrixDouble
    SMatrix(matrix)
  }

  protected final def matrix_chart(u: LispContext, p: IMatrix[Double]): SExpr = {
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0,1.0)
    p += plot(x, x :^ 2.0)
    p += plot(x, x :^ 3.0, '.')
    p.xlabel = "x axis"
    p.ylabel = "y axis"
    f.refresh()
    SProcess(MatrixPart.BreezeFigureProcess(f))
  }
}

object MatrixPart {
  case class BreezeFigureProcess(figure: Figure) extends IProcess {
  }

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
