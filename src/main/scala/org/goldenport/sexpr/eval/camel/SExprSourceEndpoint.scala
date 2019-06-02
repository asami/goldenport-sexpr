package org.goldenport.sexpr.eval

import org.apache.camel._
import org.apache.camel.impl.{DefaultEndpoint, DefaultConsumer} // Camel 2.23.1 (not 3.0.0)
import org.goldenport.sexpr._

/*
 * @since   Mar. 18, 2019
 * @version Mar. 18, 2019
 * @author  ASAMI, Tomoharu
 */
class SExprSourceEndpoint(val sexpr: SExpr) extends DefaultEndpoint() {
  import SExprSourceEndpoint._

  private var _is_done: Boolean = false

  def done(): Unit = _is_done = true

  def isSingleton(): Boolean = false

  def createProducer(): Producer = throw new UnsupportedOperationException()

  def createConsumer(processor: Processor): Consumer =
    if (_is_done)
      new IdleSourceConsumer(this, processor)
    else
      new SExprSourceConsumer(this, processor)
}

object SExprSourceEndpoint {
  class SExprSourceConsumer(
    val endpoint: SExprSourceEndpoint,
    val processor: Processor
  ) extends DefaultConsumer(endpoint, processor) {
    override protected def doStart() = {
      super.doStart()
      val exchange = endpoint.createExchange()
      val body = endpoint.sexpr match {
        case m: SString => m.asString
        case m => m.asString
      }
      exchange.getIn().setBody(body)
      processor.process(exchange)
      endpoint.done()
    }
  }

  class IdleSourceConsumer(
    val endpoint: SExprSourceEndpoint,
    val processor: Processor
  ) extends DefaultConsumer(endpoint, processor) {
  }
}
