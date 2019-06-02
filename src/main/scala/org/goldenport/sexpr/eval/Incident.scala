package org.goldenport.sexpr.eval

import org.goldenport.value._
import org.goldenport.cli.ShellCommand
import org.goldenport.bag.ChunkBag
import org.goldenport.record.v3.Record
import org.goldenport.record.http
import org.goldenport.sexpr._
import Incident._

/*
 * @since   Apr. 12, 2019
 * @version Apr. 19, 2019
 * @author  ASAMI, Tomoharu
 */
trait Incident {
  def kind: Kind
  def bindings: Record
  def show: String
}

case class ShellCommandIncident(
  result: Either[Throwable, ShellCommand.Result]
) extends Incident {
  val kind = ShellCommandKind

  lazy val stdout = result match {
    case Right(r) => Some(SBlob(r.stdout))
    case Left(l) => None
  }

  lazy val stderr = result match {
    case Right(r) => Some(SClob(r.stderr))
    case Left(l) => None
  }

  lazy val bindings: Record = {
    val r = result match {
      case Right(r) => r.waitFor
      case Left(l) => -1
    }
    val retval = SNumber(r)
    val code = result match {
      case Right(r) => r.waitFor match {
        case 0 => 200
        case _ => _parse_error(r.stderr)
      }
      case Left(l) => -1
    }
    Record.data(
      PROP_INCIDENT_KIND -> SString(kind.name),
      PROP_IS_SUCCESS -> SBoolean(r == 0),
      PROP_RETURN_VALUE -> retval,
      PROP_RETURN_CODE -> SNumber(code)
    ) + Record.dataOption(
      "stdout" -> stdout,
      "stderr" -> stderr
    )
  }

  private def _parse_error(p: ChunkBag): Int = 500 // TODO

  def show = kind.name // TODO

  def isNotFound = stderr.fold(false)(_.text.contains("command not found"))
}
object ShellCommandIncident {
  def apply(result: ShellCommand.Result): ShellCommandIncident = ShellCommandIncident(Right(result))
  def apply(e: Throwable): ShellCommandIncident = ShellCommandIncident(Left(e))
}

case class RestIncident(
  request: http.Request,
  response: Either[Throwable, http.Response]
) extends Incident {
  val kind = ShellCommandKind

  val code = response match {
    case Right(r) => SNumber(r.code)
    case Left(l) => SNumber(500)
  }

  val issuccess = response match {
    case Right(r) => SBoolean(r.isSuccess)
    case Left(l) => SBoolean.FALSE
  }

  lazy val bindings: Record = {
    Record.data(
      PROP_INCIDENT_KIND -> SString(kind.name),
      PROP_IS_SUCCESS -> issuccess,
      PROP_RETURN_VALUE -> code,
      PROP_RETURN_CODE -> code,
      "request-url" -> SUrl(request.url),
      "request-method" -> SString(request.method.name),
      "response-code" -> code
    )
  }

  def show = s"${request.url} => ${_show_result}"

  private def _show_result = response match {
    case Left(e) => e.toString
    case Right(res) => res.code.toString
  }
}
object RestIncident {
  def apply(req: http.Request, res: http.Response): RestIncident =
    RestIncident(req, Right(res))

  def apply(req: http.Request, e: Throwable): RestIncident =
    RestIncident(req, Left(e))
}

object Incident {
  final val PROP_INCIDENT_KIND = "incidnet-kind"
  final val PROP_IS_SUCCESS = "is-success"
  final val PROP_RETURN_VALUE = "return-value"
  final val PROP_RETURN_CODE = "return-code"

  trait Kind extends NamedValueInstance
  object Kind extends EnumerationClass[Kind] {
    val elements = Vector(ShellCommandKind, RestKind)
  }

  case object ShellCommandKind extends Kind {
    val name = "shell-command"
  }

  case object RestKind extends Kind {
    val name = "rest"
  }
}
