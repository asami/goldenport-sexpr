package org.goldenport.sexpr.eval

import scala.util.control.NonFatal

/*
 * @since   Sep. 21, 2018
 *  version Mar. 10, 2019
 * @version Jul. 14, 2019
 * @author  ASAMI, Tomoharu
 */
case class FunctionSpecification(
  name: String,
  numberOfMeaningfulParameters: Int = 0
) {
  def label(e: Throwable): String = {
    val a = e match {
      case m: IllegalArgumentException => _message("Illegal Argument", m)
      case m => try {
        m.toString
      } catch {
        case NonFatal(e) => m.getClass.getSimpleName
      }
    }
    s"$name($numberOfMeaningfulParameters): $a"
  }

  private def _message(label: String, e: Throwable) = 
    Option(e.getMessage).map(x => s"$label: $x").getOrElse(label)
}

object FunctionSpecification {
  case class Signature()
  case class Parameters()
}
