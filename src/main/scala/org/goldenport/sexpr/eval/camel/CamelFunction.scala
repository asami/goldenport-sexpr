package org.goldenport.sexpr.eval.camel

import scala.concurrent.Future
import scala.concurrent.duration._
import org.apache.camel.main.Main
import org.apache.camel.builder.RouteBuilder
import org.goldenport.exception.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Mar. 18, 2019
 * @version Mar. 21, 2019
 * @author  ASAMI, Tomoharu
 */
object CamelFunction {
  val functions = Vector(Camel)

  case object Camel extends AsyncIoFunction {
    val specification = FunctionSpecification("camel", 1)

    def apply(p: LispContext): LispContext = {
      val timeout = 60.second // TODO
      val in = p.parameters.argument1[SExpr](specification)
      val route = new SExprRoute(p, in)
      val camel = new Main()
      camel.addRouteBuilder(route)
      camel.start
      p.toResult(SWait("camel", route.outcome, timeout))
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  // def x = {
  //   val camel = new Main()
  //   camel.addRouteBuilder(new SExprRoute(???))
  //   camel.start()
  // }

  class SExprRoute(ctx: LispContext, in: SExpr) extends RouteBuilder {
    import scala.concurrent.ExecutionContext.Implicits.global // TODO

    val sink = new SExprSinkEndpoint()
    val outcome: Future[LispContext] = sink.process.result.map(ctx.toResult)

    override def configure() {
      val src = new SExprSourceEndpoint(in)
      val a = from(src)
      a.to(sink)
    }
  }
}
