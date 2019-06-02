package org.goldenport.sexpr.eval

import org.apache.camel._
import org.apache.camel.impl.{DefaultEndpoint, DefaultProducer} // Camel 2.23.1 (not 3.0.0)
import org.goldenport.sexpr._

/*
 * @since   Mar. 18, 2019
 * @version Mar. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class SExprSinkEndpoint() extends DefaultEndpoint() {
  import SExprSinkEndpoint._

  val process = new PromiseProcess()

  def isSingleton(): Boolean = false

  def createProducer(): Producer = new SExprSinkProducer(this, process)

  def createConsumer(processor: Processor): Consumer = throw new UnsupportedOperationException()
}

object SExprSinkEndpoint {
  class SExprSinkProducer(
    val endpoint: SExprSinkEndpoint,
    val process: PromiseProcess
  ) extends DefaultProducer(endpoint) {
    def process(p: Exchange): Unit = {
      val payload = p.getIn().getBody()
      val r = SExpr.create(payload)
      process.success(r)
    }
  }
}
