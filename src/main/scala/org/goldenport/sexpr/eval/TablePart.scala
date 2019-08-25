package org.goldenport.sexpr.eval

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.io.{ResourceHandle, MimeType}
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v2.bag.{CsvBag, ExcelBag, RecordBag}
import org.goldenport.record.v3.{ITable, Table, Record}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.chart.Chart
import org.goldenport.util.StringUtils

/*
 * @since   Feb.  9, 2019
 *  version Feb. 12, 2019
 *  version Mar. 10, 2019
 *  version May. 26, 2019
 *  version Jul. 29, 2019
 * @version Aug. 18, 2019
 * @author  ASAMI, Tomoharu
 */
trait TablePart { self: LispFunction =>
  protected final def table_load(u: LispContext, p: URI): SExpr = {
    val h = u.takeResourceHandle(p)
    table_load(u, h)
  }

  protected final def table_load(u: LispContext, p: ResourceHandle): SExpr = {
    p.getMimeType.collect {
      case MimeType.TEXT_XML => table_load_sexpr(u, p)
      case MimeType.TEXT_HTML => table_load_sexpr(u, p)
      case MimeType.APPLICATION_JSON => table_load_sexpr(u, p)
      case MimeType.TEXT_CSV => table_load_csv(u, p)
      case MimeType.APPLICATION_EXCEL => table_load_excel(u, p)
    }.getOrElse(RAISE.invalidArgumentFault(s"Unknown file type: $p"))
  }

  protected final def table_load_sexpr(u: LispContext, p: ResourceHandle): STable = {
    // val sexpr = resolve_uri(u, p)
    // table_make(u, sexpr)
    ???
  }

  protected final def table_load_csv(u: LispContext, p: ResourceHandle): STable = {
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
    val csv = CsvBag.loadResource(p, strategy)
    val table = csv.toTable
    STable(table)
  }

  protected final def table_load_excel(u: LispContext, p: ResourceHandle): STable = {
    // val config = u.config
    // val strategy = CsvBag.Strategy.default.update(
    //   Some(CsvBag.Strategy.default.recordBagStrategy.update(
    //     config.getString("csv.codec").map(scalax.io.Codec.apply),
    //     None,
    //     None,
    //     None
    //   )),
    //   config.getString("csv.name"),
    //   config.getString("csv.lineEnd"),
    //   config.getBoolean("csv.isForceDoubleQuote")
    // )
    val strategy = RecordBag.Strategy.plainAuto.update(
      None,
      None,
      None,
      None
    )
    val excel = ExcelBag.loadResource(p, strategy)
    val table = excel.toTable
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
    val records = p.toRecordVector
    val schema = p.schema
    val logic = DrawChartLogic(
      u,
      p,
      schema.columns(1), // XXX
      schema.columns(2), // XXX
      Some(schema.columns(0)) // XXX
    )
    logic.draw(records)
  }

  private case class DrawChartLogic(
    u: LispContext,
    table: ITable,
    xColumn: Column,
    yColumn: Column,
    titleColumn: Option[Column]
  ) {
    def locale = u.locale
    val xLabel = xColumn.label(locale)
    val yLabel = yColumn.label(locale)
    val zlabel = None
    val title: String = titleColumn.map(_.label(locale)).getOrElse("No title")

    def draw(ps: Seq[Record]): SExpr = {
      val uselegend = false
      val usetooltip = false
      val uselabel = false
      val useurl = false
      val chart = Chart(
        Some(title),
        Some(xLabel),
        Some(yLabel),
        zlabel,
        uselabel,
        usetooltip,
        uselabel,
        useurl
      )
      val plots = _make_plots(ps)
      val space = S2DSpace(plots, Some(chart))
      u.feature.chart.draw(space)
    }

    private def _make_plots(ps: Seq[Record]): Vector[S2DSpace.Series] =
      Vector(_make_series(ps))

    private def _make_series(ps: Seq[Record]): S2DSpace.Series = {
      val name = title
      val label = None
      val elements = _make_particles(ps)
      S2DSpace.Series(name, label, elements)
    }

    private def _make_particles(ps: Seq[Record]): Seq[S2DSpace.Particle] = {
      val labelkey = titleColumn.get.name // TODO
      val xkey = xColumn.name
      val ykey = yColumn.name
      ps.flatMap(_make_particle(labelkey, xkey, ykey))
    }

    private def _make_particle(labelkey: String, xkey: String, ykey: String)(p: Record) = {
      (p.getString(labelkey), p.getDouble(xkey), p.getDouble(ykey)) match {
        case (Some(label), Some(x), Some(y)) => Some(S2DSpace.Point(x, y, label))
        case _ => None
      }
    }
  }
}

object TablePart {
}
