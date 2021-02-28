package org.goldenport.sexpr.eval

import java.net.{URI, URL}
import java.io.File
import org.goldenport.RAISE
import org.goldenport.io.{ResourceHandle, MimeType}
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v2.bag.{CsvBag, ExcelBag, RecordBag}
import org.goldenport.record.v3.{ITable, Table, Record}
import org.goldenport.values.{NumberRange, EnumRange}
import org.goldenport.xsv._
// import org.goldenport.sexpr.eval.chart.Chart
import org.goldenport.util.StringUtils
import org.goldenport.sexpr._

/*
 * @since   Feb.  9, 2019
 *  version Feb. 12, 2019
 *  version Mar. 10, 2019
 *  version May. 26, 2019
 *  version Jul. 29, 2019
 *  version Aug. 18, 2019
 *  version Sep. 19, 2019
 *  version Dec. 29, 2019
 *  version Jan. 26, 2020
 *  version Feb. 29, 2020
 * @version Feb. 20, 2021
 * @author  ASAMI, Tomoharu
 */
trait TablePart { self: LispFunction =>
  protected final def table_load(u: LispContext, p: SUri): STable = table_load(u, p.uri)

  protected final def table_load(u: LispContext, p: URI): STable = {
    val h = u.takeResourceHandle(p)
    table_load(u, h)
  }

  protected final def table_load(u: LispContext, p: SUrl): STable = table_load(u, p.url)

  protected final def table_load(u: LispContext, p: URL): STable = {
    val h = u.takeResourceHandle(p)
    table_load(u, h)
  }

  // migrate to LispContext
  protected final def table_load(u: LispContext, p: ResourceHandle): STable = {
    p.getMimeType.collect {
      case MimeType.TEXT_XML => table_load_sexpr(u, p)
      case MimeType.TEXT_HTML => table_load_sexpr(u, p)
      case MimeType.APPLICATION_JSON => table_load_sexpr(u, p)
      case MimeType.TEXT_CSV => table_load_csv(u, p)
      case MimeType.TEXT_TSV => table_load_tsv(u, p)
      case MimeType.TEXT_XSV => table_load_xsv(u, p)
      case MimeType.TEXT_LCSV => table_load_lcsv(u, p)
      case MimeType.TEXT_LTSV => table_load_ltsv(u, p)
      case MimeType.TEXT_LXSV => table_load_lxsv(u, p)
      case MimeType.APPLICATION_EXCEL => table_load_excel(u, p)
    }.getOrElse(RAISE.invalidArgumentFault(s"Unknown file type: $p"))
  }

  protected final def table_load_sexpr(u: LispContext, p: ResourceHandle): STable = {
    // val sexpr = resolve_uri(u, p)
    // table_make(u, sexpr)
    RAISE.notImplementedYetDefect
  }

  protected final def table_load_csv(u: LispContext, p: ResourceHandle): STable =
    u.loadTableCsv(p)

  // protected final def table_load_csv(u: LispContext, p: ResourceHandle): STable = {
  //   val config = u.config
  //   val strategy = CsvBag.Strategy.default.update(
  //     Some(CsvBag.Strategy.default.recordBagStrategy.update(
  //       config.getString("csv.codec").map(scalax.io.Codec.apply),
  //       None,
  //       None,
  //       None
  //     )),
  //     config.getString("csv.name"),
  //     config.getString("csv.lineEnd"),
  //     config.getBoolean("csv.isForceDoubleQuote")
  //   )
  //   val csv = CsvBag.loadResource(p, strategy)
  //   val table = csv.toTable
  //   STable(table)
  // }

  protected final def table_load_tsv(u: LispContext, p: ResourceHandle): STable = {
    val strategy = Xsv.TsvStrategy
    val xsv = Lxsv.load(strategy, p)
    val table = Table.create(xsv)
    STable(table)
  }

  protected final def table_load_xsv(u: LispContext, p: ResourceHandle): STable = {
    val strategy = Xsv.XsvStrategy
    val xsv = Lxsv.load(strategy, p)
    val table = Table.create(xsv)
    STable(table)
  }

  protected final def table_load_lcsv(u: LispContext, p: ResourceHandle): STable = {
    val strategy = Xsv.CsvStrategy
    val lxsv = Lxsv.load(strategy, p)
    val table = Table.create(lxsv)
    STable(table)
  }

  protected final def table_load_ltsv(u: LispContext, p: ResourceHandle): STable = {
    val strategy = Xsv.TsvStrategy
    val lxsv = Lxsv.load(strategy, p)
    val table = Table.create(lxsv)
    STable(table)
  }

  protected final def table_load_lxsv(u: LispContext, p: ResourceHandle): STable = {
    val strategy = Xsv.XsvStrategy
    val lxsv = Lxsv.load(strategy, p)
    val table = Table.create(lxsv)
    STable(table)
  }

  protected final def table_load_excel(u: LispContext, p: ResourceHandle): STable =
    u.loadTableExcel(p)

  // protected final def table_load_excel(u: LispContext, p: ResourceHandle): STable = {
  //   // val config = u.config
  //   // val strategy = CsvBag.Strategy.default.update(
  //   //   Some(CsvBag.Strategy.default.recordBagStrategy.update(
  //   //     config.getString("csv.codec").map(scalax.io.Codec.apply),
  //   //     None,
  //   //     None,
  //   //     None
  //   //   )),
  //   //   config.getString("csv.name"),
  //   //   config.getString("csv.lineEnd"),
  //   //   config.getBoolean("csv.isForceDoubleQuote")
  //   // )
  //   val strategy = RecordBag.Strategy.plainAuto.update(
  //     None,
  //     None,
  //     None,
  //     None
  //   )
  //   val excel = ExcelBag.loadResource(p, strategy)
  //   val table = excel.toTable
  //   STable(table)
  // }

  protected final def table_save(u: LispContext, uri: SUri, p: SExpr): SExpr =
    uri.getFile.
      map(table_save(u, _, _table(p))).
      getOrElse(RAISE.notImplementedYetDefect)

  protected final def table_save(u: LispContext, file: File, p: SExpr): SExpr =
    table_save(u, file, _table(p))

  protected final def table_save(u: LispContext, file: File, p: ITable): SExpr =
    SExpr.run {
      val data = p.matrix.columnIterator.map(_.map(_.toString)).toVector
      val strategy = csv_strategy(u.config).withSchema(p.schema)
      val csv = CsvBag.create(file, strategy)
      csv.write(data)
      SBoolean.TRUE
    }

  protected final def table_make(u: LispContext, p: SExpr): SExpr = p match {
    case m: STable => m
    case m: SHtml => table_make_html(xpath_traverse_node("//table", m))
    case m: SXml => table_make(m)
    case m: SJson => table_make(m)
    case m: SError => m
    case m => SError.invalidArgumentFault(s"$m")
  }

  protected final def table_make(p: SXml): STable = STable(Table.create(p.dom))

  protected final def table_make_html(p: SXml): STable = STable(Table.createHtml(p.dom))

  protected final def table_make(p: SJson): STable = STable(Table.create(p.json))

  protected final def table_matrix(t: ITable, p: SRange): SMatrix =
    table_matrix(t, p.range)

  protected final def table_matrix(t: ITable, p: NumberRange): SMatrix =
    SMatrix(t.matrix.select(p).toDoubleMatrix)

  protected final def table_matrix(t: ITable): SMatrix =
    SMatrix(t.matrix.makeDoubleMatrix)

  protected final def table_select(t: ITable, p: SRange): STable =
    table_select(t, p.range)

  protected final def table_select(t: ITable, p: NumberRange): STable =
    STable(t.select(p))

  protected final def table_select(t: ITable, p: Seq[Int]): STable =
    STable(t.select(NumberRange.createInt(p)))

  protected final def table_regression(u: LispContext, p: ITable): SRecord = {
    val range = EnumRange(0, 1)
    val mx = p.matrix.select(range).toDoubleMatrix
    val (c, s) = u.numericalOperations.simpleRegression(mx)
    SRecord(Record.data("intercept" -> c, "slope" -> s))
  }

  protected final def table_chart(u: LispContext, p: ITable, analyzes: List[String]): SExpr = {
    val chart = u.feature.chart.buildChart(u, p, analyzes)
    u.feature.chart.draw(chart)
  }

  private def _table(p: SExpr): ITable = p match {
    case STable(t) => t
    case m => RAISE.notImplementedYetDefect
  }

  // protected final def table_chart(u: LispContext, p: ITable): SExpr = {
  //   // val f = Figure()
  //   // val p = f.subplot(0)
  //   // val x = linspace(0.0,1.0)
  //   // p += plot(x, x :^ 2.0)
  //   // p += plot(x, x :^ 3.0, '.')
  //   // p.xlabel = "x axis"
  //   // p.ylabel = "y axis"
  //   // f.refresh()
  //   // SWindow(MatrixPart.BreezeFigureWindow(f))
  //   val records = p.toRecordVector
  //   val schema = p.schema
  //   val logic = DrawChartLogic(
  //     u,
  //     p,
  //     schema.columns(1), // XXX
  //     schema.columns(2), // XXX
  //     Some(schema.columns(0)) // XXX
  //   )
  //   logic.draw(records)
  // }

  // private case class DrawChartLogic(
  //   u: LispContext,
  //   table: ITable,
  //   xColumn: Column,
  //   yColumn: Column,
  //   titleColumn: Option[Column]
  // ) {
  //   def locale = u.locale
  //   val xLabel = xColumn.label(locale)
  //   val yLabel = yColumn.label(locale)
  //   val zlabel = None
  //   val title: String = titleColumn.map(_.label(locale)).getOrElse("No title")

  //   def draw(ps: Seq[Record]): SExpr = {
  //     val uselegend = false
  //     val usetooltip = false
  //     val uselabel = false
  //     val useurl = false
  //     val chart = Chart(
  //       Some(title),
  //       Some(xLabel),
  //       Some(yLabel),
  //       zlabel,
  //       uselabel,
  //       usetooltip,
  //       uselabel,
  //       useurl
  //     )
  //     val plots = _make_plots(ps)
  //     val space = S2DSpace(plots, Some(chart))
  //     u.feature.chart.draw(space)
  //   }

  //   private def _make_plots(ps: Seq[Record]): Vector[S2DSpace.Series] = {
  //     Vector(_make_particle_series(ps), _make_line_series(ps))
  //   }

  //   private def _make_particle_series(ps: Seq[Record]): S2DSpace.Series = {
  //     val name = title
  //     val label = None
  //     val elements = _make_particles(ps)
  //     S2DSpace.Series.shape(name, label, elements)
  //   }

  //   private def _make_particles(ps: Seq[Record]): Seq[S2DSpace.Particle] = {
  //     val labelkey = titleColumn.get.name // TODO
  //     val xkey = xColumn.name
  //     val ykey = yColumn.name
  //     ps.flatMap(_make_particle(labelkey, xkey, ykey))
  //   }

  //   private def _make_particle(labelkey: String, xkey: String, ykey: String)(p: Record) = {
  //     (p.getString(labelkey), p.getDouble(xkey), p.getDouble(ykey)) match {
  //       case (Some(label), Some(x), Some(y)) => Some(S2DSpace.Point(x, y, label))
  //       case _ => None
  //     }
  //   }

  //   private def _make_line_series(ps: Seq[Record]): S2DSpace.Series = {
  //     val xs = Vector(_make_point(ps.head), _make_point(ps.last))
  //     S2DSpace.Series.line(xs)
  //   }

  //   private def _make_point(p: Record): S2DSpace.Point = {
  //     val xkey = xColumn.name
  //     val ykey = yColumn.name
  //     (p.getDouble(xkey), p.getDouble(ykey)) match {
  //       case (Some(x), Some(y)) => S2DSpace.Point(x, y)
  //       case _ => RAISE.noReachDefect
  //     }
  //   }
  // }
}

object TablePart {
}
