package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import java.net.{URI, URL}
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import org.goldenport.RAISE
import org.goldenport.log.LogContext
import org.goldenport.i18n.I18NContext
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.query.QueryExpression
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}
import org.goldenport.record.http.Response
import org.goldenport.log.LogMark._
import org.goldenport.cli.ShellCommand
import org.goldenport.io.{ResourceManager, ResourceHandle, ResourceLocator}
import org.goldenport.io.MimeType
import org.goldenport.bag.ChunkBag
import org.goldenport.record.v2.bag.{CsvBag, ExcelBag, RecordBag}
import org.goldenport.incident.{Incident => LibIncident}
import org.goldenport.matrix.INumericalOperations
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.store.StoreFeature
import org.goldenport.sexpr.eval.chart.ChartFeature

/*
 * @since   Sep. 15, 2018
 *  version Sep. 29, 2018
 *  version Oct. 17, 2018
 *  version Feb. 28, 2019
 *  version Mar. 31, 2019
 *  version Apr. 12, 2019
 *  version May. 20, 2019
 *  version Jun.  9, 2019
 *  version Jul. 14, 2019
 *  version Aug. 17, 2019
 *  version Sep. 30, 2019
 *  version Oct. 31, 2019
 *  version Nov.  8, 2019
 *  version Jan. 19, 2020
 *  version Feb. 29, 2020
 * @version Mar. 30, 2020
 * @author  ASAMI, Tomoharu
 */
trait LispContext extends EvalContext with ParameterPart
    with ScriptEnginePart with SparkPart {
  def config: LispConfig
  def i18nContext: I18NContext
  def evaluator: LispContext => LispContext
  def serviceLogic: UnitOfWorkLogic
  def storeLogic: StoreOperationLogic
  def scriptContext: ScriptEngineContext
  def sqlContext: SqlContext
  def resourceManager: ResourceManager
  def feature: FeatureContext
  def incident: Option[LibIncident]
  def numericalOperations: INumericalOperations

  def locale = i18nContext.locale

  def takeResourceHandle(p: ResourceLocator): ResourceHandle = resourceManager.takeHandle(p)
  def takeResourceHandle(p: URI): ResourceHandle = resourceManager.takeHandle(p)
  def takeResourceHandle(p: URL): ResourceHandle = resourceManager.takeHandle(p)

  def pure(p: SExpr): LispContext

  override def resolve: LispContext = super.resolve.asInstanceOf[LispContext]

  def reductForEval: LispContext = reductForEvalDeep

  private def reductForEvalShallow: LispContext = {
    value match {
      case xs: SList => xs.list match {
        case Nil => this
        case x :: Nil => this
        case x :: xs =>
          // log.trace(s"reduct input($x): $xs")
          val a = xs.map(x =>
            evaluator(pure(x)).value match {
              case SMute(expr) => expr
              case m => m
            }
          )
          log.trace(s"reduct input($x): $xs => $a")
          pure(SList.create(x +: a))
      }
      case _ => this
    }
  }

  private def reductForEvalDeep: LispContext = {
    case class Z(function: SExpr, c: LispContext, xs: Vector[SExpr] = Vector.empty) {
      def r = {
        val x = c.pure(SList.create(function +: xs))
        log.trace(s"reduct: ${x.value}")
        x
      }
      def +(rhs: SExpr) = {
        val r = evaluator(c.pure(rhs))
        val v = r.value match {
          case SMute(expr) => expr
          case m => m
        }
        val x = Z(function, r, xs :+ v)
        log.trace(s"reduct in: $rhs => ${x}")
        x
      }
    }
    value match {
      case xs: SList => xs.list match {
        case Nil => this
        case x :: Nil => this
        case x :: xs =>
          log.trace(s"reduct input($x): $xs")
          xs./:(Z(x, this))(_+_).r
      }
      case _ => this
    }
  }

  def reductForApply: LispContext = RAISE.noReachDefect

  def eval(expr: SExpr): SExpr = {
    val r = evaluator(pure(expr))
    r.value
  }

  def apply(expr: SExpr): LispContext = toResult(eval(expr))

  override final def toResult(expr: SExpr): LispContext = toResult(expr, Record.empty)

  override final def toResult(expr: SExpr, bindings: IRecord): LispContext = toResult(expr, None, bindings)

  def toResult(expr: SExpr, incident: LibIncident): LispContext = toResult(expr, Some(incident), Record.empty)

  def toResult(expr: SExpr, incident: Option[LibIncident], bindings: IRecord): LispContext

  override final def toResult(p: Response): LispContext = toResult(toSExpr(p))

  def toResult(p: (Parameters.Cursor, ValidationNel[SError, SExpr])): LispContext = {
    p._2 match {
      case Success(s) => toResult(s)
      case Failure(es) => es.toList match {
        case Nil => toResult(SNil)
        case x :: Nil => toResult(x)
        case x :: xs => toResult(SError.create(x, xs))
      }
    }
  }

  override def addBindings(bindings: IRecord): LispContext

  def pop: LispContext = RAISE.unsupportedOperationFault
  def pop(n: Int): LispContext = RAISE.unsupportedOperationFault
  def peek: SExpr = RAISE.unsupportedOperationFault
  def peek(n: Int): SExpr = RAISE.unsupportedOperationFault
  def takeHistory: SExpr = RAISE.unsupportedOperationFault
  def takeHistory(n: Int): SExpr = RAISE.unsupportedOperationFault
  def takeCommandHistory: SExpr = RAISE.unsupportedOperationFault
  def takeCommandHistory(n: Int): SExpr = RAISE.unsupportedOperationFault
  def getPipelineIn: Option[SExpr] = None

  val futureDuration = 10.minutes // TODO customizable

  def futureForEval(p: LispFunction): Future[LispContext] = Future {
    try {
      LogContext.setRootLevel(config.logLevel)
      log.debug(ThreadLocation, StartAction, "future", p.name)
      val r = p(this.reductForEval)
      log.debug(ThreadLocation, EndAction, "future", s"${p.name} => $r")
      r
    } catch {
      case e: Throwable =>
        log.error(ThreadLocation, EndErrorAction, "future", p.name, e)
        toResult(SError(e))
    }
  }

  def future(label: String, body: () => LispContext): Future[LispContext] = Future {
    try {
      LogContext.setRootLevel(config.logLevel)
      log.debug(ThreadLocation, StartAction, "future", label)
      val r = body()
      log.debug(ThreadLocation, EndAction, "future", s"${label} => $r")
      r
    } catch {
      case e: Throwable =>
        log.error(ThreadLocation, EndErrorAction, "future", label, e)
        toResult(SError(e))
    }
  }

  def wait(ctx: LispContext, p: Future[LispContext]): LispContext = try {
    Await.result(p, futureDuration)
  } catch {
    case NonFatal(e) => ctx.pure(SError(e))
  }

  def withUnavailableFunction(p: String) = this // TODO

  def isShellCommand(p: String): Boolean = ShellCommand.run(s"type $p")

  def isUnavailableFunction(p: String): Boolean = false

  def createDynamicServiceFunction(name: String): DynamicServiceFunction =
    DynamicServiceFunction.create(name)

  def format(p: SExpr): SString = SString(formatString(p))

  def format(rule: String, p: SExpr): SString = SString(formatString(rule, p))

  def formatString(p: SExpr): String = p match {
    case SString(s) => s
    case m => i18nContext.format(m.asObject)
  }

  def formatString(rule: String, p: SExpr): String = i18nContext.format(rule, p.asObject)

  def formatMessage(rule: String, ps: Seq[SExpr]): String = i18nContext.formatMessage(rule, ps.map(_.asObject))

  def formatMessageKey(key: String, ps: Seq[SExpr]): String = i18nContext.formatMessageKey(key, ps.map(_.asObject))

  def resolveUrn(urn: SUrn): SExpr = urn // TODO driver

  def unmarshall(url: URL, p: ChunkBag): SExpr = unmarshall(MimeType.getBySuffix(url), p)

  def unmarshall(mime: Option[MimeType], p: ChunkBag): SExpr =
    mime.map(unmarshall(_, p)).getOrElse(SBlob(p))

  def unmarshall(mime: MimeType, p: ChunkBag): SExpr = { // TODO customizable
    def stringorclob = p.getSize.map(size =>
      if (size > 8192)
        SString(p.toText)
      else
        SClob(p)
    ).getOrElse(SClob(p))
    _unmarshall_option(mime, p).getOrElse(
      if (mime.isText)
        stringorclob
      else
        SBlob(p)
    )
  }

  def unmarshall(mime: MimeType, p: String): SExpr = { // TODO customizable
    _unmarshall_option(mime, p).getOrElse(SString(p))
  }

  private def _unmarshall_option(mime: MimeType, p: ChunkBag): Option[SExpr] =
    Option(mime).collect {
      case MimeType.TEXT_CSV => loadTableCsv(p)
      case MimeType.APPLICATION_EXCEL => loadTableExcel(p)
    }.orElse(_unmarshall_option(mime, p.toText))

  private def _unmarshall_option(mime: MimeType, p: => String): Option[SExpr] =
    Option(mime) collect {
      case MimeType.TEXT_XSL => SXsl(p)
      case MimeType.TEXT_CSV => loadTableCsv(p) // XXX
      case m if m.isHtml => SHtml(p)
      case m if m.isXml => SXml(p)
      case m if m.isJson => SJson(p)
    }

  def loadTable(p: SUri): STable = loadTable(p.uri)

  def loadTable(p: URI): STable = loadTable(takeResourceHandle(p))

  def loadTable(p: SUrl): STable = loadTable(p.url)

  def loadTable(p: URL): STable = loadTable(takeResourceHandle(p))

  def loadTable(p: ResourceHandle): STable = {
    p.getMimeType.collect {
      case MimeType.TEXT_XML => loadTableSExpr(p)
      case MimeType.TEXT_HTML => loadTableSExpr(p)
      case MimeType.APPLICATION_JSON => loadTableSExpr(p)
      case MimeType.TEXT_CSV => loadTableCsv(p)
      case MimeType.TEXT_TSV => loadTableTsv(p)
      case MimeType.TEXT_XSV => loadTableXsv(p)
      case MimeType.TEXT_LCSV => loadTableLcsv(p)
      case MimeType.TEXT_LTSV => loadTableLtsv(p)
      case MimeType.TEXT_LXSV => loadTableLxsv(p)
      case MimeType.APPLICATION_EXCEL => loadTableExcel(p)
    }.getOrElse(RAISE.invalidArgumentFault(s"Unknown file type: $p"))
  }

  def loadTableSExpr(p: ResourceHandle): STable = {
    // val sexpr = resolve_uri(u, p)
    // table_make(u, sexpr)
    RAISE.notImplementedYetDefect
  }

  // See UtilityPart#csv_strategy
  private lazy val _csv_strategy = CsvBag.Strategy.default.update(
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

  def loadTableCsv(p: ResourceHandle): STable = {
    val csv = CsvBag.loadResource(p, _csv_strategy)
    val table = csv.toTable
    STable(table)
  }

  def loadTableCsv(p: ChunkBag): STable = {
    val csv = CsvBag.create(p, _csv_strategy)
    val table = csv.toTable
    STable(table)
  }

  def loadTableCsv(p: String): STable = { // TODO don't work. See StringInputStream?
    val csv = CsvBag.createFromString(p, _csv_strategy)
    val table = csv.toTable
    STable(table)
  }

  def loadTableTsv(p: ResourceHandle): STable = {
    RAISE.notImplementedYetDefect
  }

  def loadTableXsv(p: ResourceHandle): STable = {
    RAISE.notImplementedYetDefect
  }

  def loadTableLcsv(p: ResourceHandle): STable = {
    RAISE.notImplementedYetDefect
  }

  def loadTableLtsv(p: ResourceHandle): STable = {
    RAISE.notImplementedYetDefect
  }

  def loadTableLxsv(p: ResourceHandle): STable = {
    RAISE.notImplementedYetDefect
  }

  private lazy val _excel_strategy = RecordBag.Strategy.plainAuto.update(
    None,
    None,
    None,
    None
  )

  def loadTableExcel(p: ResourceHandle): STable = {
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
    val excel = ExcelBag.loadResource(p, _excel_strategy)
    val table = excel.toTable
    STable(table)
  }

  def loadTableExcel(p: ChunkBag): STable = {
    val strategy = _excel_strategy
    val name = "unknown"
    val file = p.switchToFileBag
    val excel = ExcelBag.create(
      ExcelBag.Xlsx,
      file,
      strategy,
      name
    )
    val table = excel.toTable
    STable(table)
  }
}

object LispContext {
  val defaultServiceLogic = UnitOfWorkLogic.printer
  val defaultStoreLogic = StoreOperationLogic.printer
  // def defaultFeature = FeatureContext.default

  def apply(
    config: LispConfig,
    i18ncontext: I18NContext,
    querycontext: QueryExpression.Context,
    evaluator: LispContext => LispContext, // apply_context
    x: SExpr
  ): LispContext = {
    val scriptContext = ScriptEngineContext.default
    val sqlcontext = {
      if (true)
        SqlContext.createEachTime(config.properties, querycontext)
      else if (false)
        SqlContext.createAutoCommit(config.properties, querycontext)
      else
        SqlContext.createConnectionPool(config.properties, querycontext)
    }
    val resourceManager = new ResourceManager()
    val numericalOperations = config.numericalOperations
    val featurecontext = FeatureContext(
      new StoreFeature(config.properties, i18ncontext, sqlcontext),
      ChartFeature.default
    )
    PlainLispContext(
      config,
      i18ncontext,
      evaluator,
      defaultServiceLogic,
      defaultStoreLogic,
      scriptContext,
      sqlcontext,
      resourceManager,
      numericalOperations,
      featurecontext,
      x,
      None,
      Record.empty
    )
  }

  case class PlainLispContext(
    config: LispConfig,
    i18nContext: I18NContext,
    evaluator: LispContext => LispContext,
    serviceLogic: UnitOfWorkLogic,
    storeLogic: StoreOperationLogic,
    scriptContext: ScriptEngineContext,
    sqlContext: SqlContext,
    resourceManager: ResourceManager,
    numericalOperations: INumericalOperations,
    feature: FeatureContext,
    value: SExpr,
    incident: Option[LibIncident],
    bindings: Record
  ) extends LispContext {
    def pure(p: SExpr) = copy(value = p)
    def toResult(p: SExpr, i: Option[LibIncident], b: IRecord) = copy(value = p, incident = i, bindings = bindings + b.toRecord)
    def addBindings(p: IRecord) = copy(bindings = bindings + p.toRecord)
  }
}
