package org.goldenport.sexpr.eval

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.record.v2.bag.CsvBag
import org.goldenport.record.v3.Table
import org.goldenport.table.ITable
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.chart.Chart

/*
 * @since   Feb.  9, 2019
 *  version Feb. 12, 2019
 *  version Mar. 10, 2019
 *  version May. 26, 2019
 * @version Jul. 29, 2019
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

  protected final def table_make(u: LispContext, p: SExpr): STable = p match {
    case m: STable => m
    case m: SHtml => table_make_html(xpath_traverse_node("//table", m))
    case m: SXml => table_make(m)
    case m: SJson => table_make(m)
    case m => RAISE.notImplementedYetDefect
  }

  protected final def table_make(p: SXml): STable = STable(Table.create(p.dom))

  protected final def table_make_html(p: SXml): STable = STable(Table.createHtml(p.dom))

  protected final def table_make(p: SJson): STable = STable(Table.create(p.json))

  protected final def table_chart(u: LispContext, p: ITable): SExpr = {
    // val f = Figure()
    // val p = f.subplot(0)
    // val x = linspace(0.0,1.0)
    // p += plot(x, x :^ 2.0)
    // p += plot(x, x :^ 3.0, '.')
    // p.xlabel = "x axis"
    // p.ylabel = "y axis"
    // f.refresh()
    // SWindow(MatrixPart.BreezeFigureWindow(f))
    val chart = Chart.empty
    val plots = Vector.empty
    val space = S2DSpace(plots, chart)
    u.feature.chart.draw(space)
  }
}

object TablePart {
}
