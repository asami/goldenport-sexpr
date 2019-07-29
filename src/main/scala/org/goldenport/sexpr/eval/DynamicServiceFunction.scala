package org.goldenport.sexpr.eval

import scala.util.control.NonFatal
import org.goldenport.RAISE
import org.goldenport.cli.ShellCommand
import org.goldenport.record.v3.Record
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.http
import org.goldenport.util.StringUtils
import org.goldenport.sexpr._

/*
 * @since   Oct.  1, 2018
 *  version Oct. 12, 2018
 *  version Feb. 27, 2019
 *  version Mar.  4, 2019
 *  version Apr. 12, 2019
 * @version Jun.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class DynamicServiceFunction(
  specification: FunctionSpecification
) extends IoFunction {
  def apply(p: LispContext): LispContext =
    applyOption(p).getOrElse(p.toResult(SError.functionNotFound(p.evalElements.functionName)))

  private def applyOption(p: LispContext): Option[LispContext] = 
    _invoke_shell_command_option(p) orElse _invoke_rest_service_option(p)

  def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect

  private def _invoke_shell_command_option(p: LispContext): Option[LispContext] = {
    val elements = p.evalElements
    val functionname = elements.functionName
    val r = if (p.isUnavailableFunction(functionname) || !p.isShellCommand(functionname))
      None
    else
      _invoke_shell_command(p)
    log_trace(s"""DynamicServiceFunction#_invoke_shell_command_option($functionname => $r""")
    r
  }

  private def _invoke_shell_command(p: LispContext): Option[LispContext] = {
    val start = System.currentTimeMillis
    val elements = p.evalElements
    val name = elements.functionName
    val commands = name :: elements.parameters.asStringList
    val env = Map.empty[String, String] // TODO
    val in = p.getPipelineIn.flatMap(_.getInputSource)
    val result = new ShellCommand(commands, env, None, in, None).run
    val w = SWait(name, { () =>
      val code = result.waitFor
      if (code == 0)
        _success(start, p, result)
      else
        _failure(start, p, result)
    })
    val r = w.resolveContext
    r.incident.flatMap {
      case m: ShellCommandIncident =>
        if (m.isNotFound)
          None
        else
          Some(r)
      case _ => Some(r)
    }
  }

  private def _success(start: Long, c: LispContext, p: ShellCommand.Result) = {
    val (incident, stdout) = _properties(start, p)
    c.toResult(stdout, incident)
  }

  private def _failure(start: Long, c: LispContext, p: ShellCommand.Result) = {
    val (incident, _) = _properties(start, p)
    val error = SError(s"Shell Command: ${p.waitFor}", incident)
    c.toResult(error, incident)
  }

  private def _properties(start: Long, p: ShellCommand.Result): (Incident, SBlob) = {
    // val retval = SNumber(p.waitFor)
    val stdout = SBlob(p.stdout)
    // val stderr = SBlob(p.stderr)
    // val a = Record.data(
    //   "return-code" -> retval,
    //   "stdout" -> stdout,
    //   "stderr" -> stderr
    // )
    val a = ShellCommandIncident(start, p)
    (a, stdout)
  }

  private def _invoke_rest_service_option(p: LispContext): Option[LispContext] = {
    val elements = p.evalElements
    val functionname = elements.functionName
    val r = if (p.isUnavailableFunction(functionname) && is_implicit_http_communication(p))
      Some(_invoke_rest_service(p))
    else
      None
    log_trace(s"""DynamicServiceFunction#_invoke_rest_service_option($functionname => $r""")
    r
  }

  private def _invoke_rest_service(p: LispContext): LispContext = http_get(p)

  // private def _invoke_rest_service_old(p: LispContext): LispContext = {
  //   // println("_invoke_rest_service")
  //   val elements = p.evalElements
  //   val functionname = elements.functionName
  //   val baseurl = "http://jsonplaceholder.typicode.com/" // TODO
  //   val future = SFutureWait.create(functionname, p) {
  //     val req = http.Request.create(baseurl, functionname)
  //     try {
  //       // println(s"_invoke_rest_service => $req")
  //       val res = p.serviceLogic.httpService(req)
  //       // println(s"_invoke_rest_service <= $res, ${res.isNotFound}")
  //       val i = RestIncident(req, res)
  //       if (res.isSuccess)
  //         p.toResult(???, i)
  //       else if (res.isNotFound)
  //         p.toResult(SError.functionNotFound(functionname)).
  //           withUnavailableFunction(functionname)
  //       else
  //         p.toResult(SError(i), i)
  //     } catch {
  //       case NonFatal(e) =>
  //         val i = RestIncident(req, e)
  //         p.toResult(SError(i), i)
  //     }
  //   }
  //   p.toResult(future.start)
  // }
}

object DynamicServiceFunction {
  def create(name: String): DynamicServiceFunction = DynamicServiceFunction(
    FunctionSpecification(name, 0)
  )
}
