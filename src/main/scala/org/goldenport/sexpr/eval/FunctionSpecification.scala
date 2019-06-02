package org.goldenport.sexpr.eval

/*
 * @since   Sep. 21, 2018
 * @version Mar. 10, 2019
 * @author  ASAMI, Tomoharu
 */
case class FunctionSpecification(
  name: String,
  numberOfMeaningfulParameters: Int = 0
) {
  def label(e: Throwable): String = {
    val a = e match {
      case m: IllegalArgumentException => _message("Illegal Argument", m)
      case m => m.toString
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
