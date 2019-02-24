package org.goldenport.sexpr.eval

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import javax.script.ScriptEngineManager
import org.goldenport.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}
import org.goldenport.record.http.Response
import org.goldenport.log.LogMark._
import org.goldenport.cli.ShellCommand
import org.goldenport.sexpr._

/*
 * @since   Sep. 15, 2018
 *  version Sep. 29, 2018
 *  version Oct. 17, 2018
 * @version Feb. 11, 2019
 * @author  ASAMI, Tomoharu
 */
trait LispContext extends EvalContext with ScriptEnginePart {
  def config: LispConfig
  def evaluator: LispContext => LispContext
  def serviceLogic: UnitOfWorkLogic
  def storeLogic: StoreOperationLogic
  def scriptContext: ScriptEngineManager

  def pure(p: SExpr): LispContext

  override def resolve: LispContext = super.resolve.asInstanceOf[LispContext]

  def reductForEval: LispContext = {
    case class Z(function: SExpr, c: LispContext, xs: Vector[SExpr] = Vector.empty) {
      def r = {
        val x = c.pure(SList.create(function +: xs))
        log.trace(s"reduct: ${x.value}")
        x
      }
      def +(rhs: SExpr) = {
        val r = evaluator(c.pure(rhs))
        val x = Z(function, r, xs :+ r.value)
        log.trace(s"reduct in: ${x}")
        x
      }
    }
    args match {
      case Nil => this
      case x :: Nil => this
      case x :: xs =>
        log.trace(s"reduct input: $args")
        xs./:(Z(x, this))(_+_).r
    }
  }

  def reductForApply: LispContext = ???

  override def toResult(expr: SExpr): LispContext = toResult(expr, Record.empty)
  override def toResult(expr: SExpr, bindings: IRecord): LispContext

  override def toResult(p: Response): LispContext = toResult(toSExpr(p))

  def pop: LispContext = RAISE.unsupportedOperationFault
  def getPipelineIn: Option[SExpr] = None

  val futureDuration = 10.minutes // TODO customizable

  def futureForEval(p: LispFunction): Future[LispContext] = Future {
    try {
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

  def wait(p: Future[LispContext]): LispContext = Await.result(p, futureDuration)

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

  def apply(
    config: LispConfig,
    evaluator: LispContext => LispContext,
    x: SExpr
  ): LispContext = PlainLispContext(
    config,
    evaluator,
    defaultServiceLogic,
    defaultStoreLogic,
    scriptContext,
    x,
    Record.empty
  )

  case class PlainLispContext(
    config: LispConfig,
    evaluator: LispContext => LispContext,
    serviceLogic: UnitOfWorkLogic,
    storeLogic: StoreOperationLogic,
    scriptContext: ScriptEngineManager,
    value: SExpr,
    bindings: IRecord
  ) extends LispContext {
    def pure(p: SExpr) = copy(value = p)
    def toResult(p: SExpr, b: IRecord) = copy(value = p, bindings = bindings + b)
  }
}
