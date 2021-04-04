package org.goldenport.sexpr.eval

// import org.goldenport.parser.ParseFailure
import org.goldenport.context._
import org.goldenport.trace._

/*
 * @since   Mar. 24, 2021
 * @version Mar. 25, 2021
 * @author  ASAMI, Tomoharu
 */
trait TracePart { self: LispContext =>
  object trace {
    // def argumentFault(p: ParseFailure[_]): Unit = {
    //   traceContext.argumentFault(p)
    //   ???
    // }
    def trace(p: Trace): Unit = traceContext.trace(p)
    def trace(ps: Seq[Trace]): Unit = traceContext.trace(ps)
    def fault(p: Fault): Unit = traceContext.fault(p)
    def fault(ps: Seq[Fault]): Unit = traceContext.fault(ps)
  }
}
