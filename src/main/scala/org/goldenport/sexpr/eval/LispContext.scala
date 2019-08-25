package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import java.net.URI
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import javax.script.ScriptEngineManager
import org.goldenport.RAISE
import org.goldenport.log.LogContext
import org.goldenport.i18n.I18NContext
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}
import org.goldenport.record.http.Response
import org.goldenport.log.LogMark._
import org.goldenport.cli.ShellCommand
import org.goldenport.io.{ResourceManager, ResourceHandle, ResourceLocator}
import org.goldenport.incident.{Incident => LibIncident}
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
 * @version Aug. 17, 2019
 * @author  ASAMI, Tomoharu
 */
trait LispContext extends EvalContext with ParameterPart with ScriptEnginePart {
  def config: LispConfig
  def i18nContext: I18NContext
  def evaluator: LispContext => LispContext
  def serviceLogic: UnitOfWorkLogic
  def storeLogic: StoreOperationLogic
  def scriptContext: ScriptEngineManager
  def sqlContext: SqlContext
  def resourceManager: ResourceManager
  def feature: FeatureContext
  def incident: Option[LibIncident]

  def locale = i18nContext.locale

  def takeResourceHandle(p: ResourceLocator): ResourceHandle = resourceManager.takeHandle(p)
  def takeResourceHandle(p: URI): ResourceHandle = resourceManager.takeHandle(p)

  def pure(p: SExpr): LispContext

  override def resolve: LispContext = super.resolve.asInstanceOf[LispContext]

  def reductForEval: LispContext = {
    value match {
      case xs: SList => xs.list match {
        case Nil => this
        case x :: Nil => this
        case x :: xs =>
          // log.trace(s"reduct input($x): $xs")
          val a = xs.map(x => evaluator(pure(x)).value)
          pure(SList.create(x +: a))
      }
      case _ => this
    }
  }

  def reductForEval0: LispContext = {
    case class Z(function: SExpr, c: LispContext, xs: Vector[SExpr] = Vector.empty) {
      def r = {
        val x = c.pure(SList.create(function +: xs))
        log.trace(s"reduct: ${x.value}")
        x
      }
      def +(rhs: SExpr) = {
        val r = evaluator(c.pure(rhs))
        val x = Z(function, r, xs :+ r.value)
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

  def reductForApply: LispContext = ???

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
}
object LispContext {
  val scriptContext = new ScriptEngineManager()
  val defaultServiceLogic = UnitOfWorkLogic.printer
  val defaultStoreLogic = StoreOperationLogic.printer
  // def defaultFeature = FeatureContext.default

  def apply(
    config: LispConfig,
    i18ncontext: I18NContext,
    evaluator: LispContext => LispContext,
    x: SExpr
  ): LispContext = {
    val sqlcontext = {
      if (true)
        SqlContext.createSync(config.properties)
      else
        SqlContext.createConnectionPool(config.properties)
    }
    val resourceManager = new ResourceManager()
    val featurecontext = FeatureContext(
      new StoreFeature(config.properties, sqlcontext),
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
    scriptContext: ScriptEngineManager,
    sqlContext: SqlContext,
    resourceManager: ResourceManager,
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
