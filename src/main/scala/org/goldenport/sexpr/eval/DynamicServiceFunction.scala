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
 * @version Oct. 12, 2018
 * @author  ASAMI, Tomoharu
 */
case class DynamicServiceFunction(
  specification: FunctionSpecification
) extends ControlFunction {
  def apply(p: LispContext): LispContext = {
    val a: Option[LispContext] = _invoke_shell_command_option(p) orElse _invoke_rest_service_option(p)
    a.getOrElse(RAISE.syntaxErrorFault(p.parameters.show))
  }

  def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect

  private def _invoke_shell_command_option(p: LispContext): Option[LispContext] = {
    val elements = p.evalElements
    val functionname = elements.functionName
    if (p.isUnavailableFunction(functionname) || !p.isShellCommand(functionname))
      None
    else
      Some(_invoke_shell_command(p))
  }

  private def _invoke_shell_command(p: LispContext): LispContext = {
    val elements = p.evalElements
    val name = elements.functionName
    val commands = name :: elements.parameters.asStringList
    val env = Map.empty[String, String] // TODO
    val in = p.getPipelineIn.flatMap(_.getInputSource)
    val result = new ShellCommand(commands, env, None, in, None).run
    val r = SWait(name, { () =>
      val code = result.waitFor
      if (code == 0)
        _success(p, result)
      else
        _failure(p, result)
    })
    p.toResult(r)
  }

  private def _success(c: LispContext, p: ShellCommand.Result) = {
    val (props, stdout) = _properties(p)
    c.toResult(stdout, props)
  }

  private def _failure(c: LispContext, p: ShellCommand.Result) = {
    val (props, _) = _properties(p)
    val error = SError(s"Shell Command: ${p.waitFor}")
    c.toResult(error, props)
  }

  private def _properties(p: ShellCommand.Result): (Record, SBag) = {
    val retval = SNumber(p.waitFor)
    val stdout = SBag(p.stdout)
    val stderr = SBag(p.stderr)
    val a = Record.data(
      "return-code" -> retval,
      "stdout" -> stdout,
      "stderr" -> stderr
    )
    (a, stdout)
  }

  private def _invoke_rest_service_option(p: LispContext): Option[LispContext] = {
    val elements = p.evalElements
    val functionname = elements.functionName
    if (p.isUnavailableFunction(functionname))
      None
    else
      Some(_invoke_rest_service(p))
  }

  private def _invoke_rest_service(p: LispContext): LispContext = {
    // println("_invoke_rest_service")
    val elements = p.evalElements
    val functionname = elements.functionName
    val baseurl = "http://jsonplaceholder.typicode.com/" // TODO
    val future = SFutureWait.create(functionname, p) {
      val req = http.Request.create(baseurl, functionname)
      try {
        // println(s"_invoke_rest_service => $req")
        val res = p.serviceLogic.httpService(req)
        println(s"_invoke_rest_service <= $res, ${res.isNotFound}")
        if (res.isSuccess)
          p.toResult(res)
        else if (res.isNotFound)
          p.toResult(SError.functionNotFound(functionname)).
            withUnavailableFunction(functionname)
        else
          p.toResult(SError(req, res))
      } catch {
        case NonFatal(e) => p.toResult(SError(req, e))
      }
    }
    p.toResult(future.start)
  }
}

object DynamicServiceFunction {
  def create(name: String): DynamicServiceFunction = DynamicServiceFunction(
    FunctionSpecification(name, 0)
  )
}
