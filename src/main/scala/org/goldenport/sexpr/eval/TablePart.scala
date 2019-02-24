package org.goldenport.sexpr.eval

import java.net.URI
import breeze.linalg._
import breeze.plot._
import org.goldenport.record.v2.bag.CsvBag
import org.goldenport.sexpr._
import org.goldenport.table.ITable

/*
 * @since   Feb.  9, 2019
 * @version Feb. 12, 2019
 * @author  ASAMI, Tomoharu
 */
trait TablePart { self: LispFunction =>
  protected final def table_load(u: LispContext, p: URI): SExpr = {
    val config = u.config
    val strategy = CsvBag.Strategy.default.update(
      Some(CsvBag.Strategy.default.recordBagStrategy.update(
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
    val table = csv.toTable
    STable(table)
  }

  protected final def table_chart(u: LispContext, p: ITable): SExpr = {
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

object TablePart {
}
