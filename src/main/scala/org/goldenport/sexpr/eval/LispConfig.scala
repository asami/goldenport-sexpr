package org.goldenport.sexpr.eval

import ch.qos.logback.classic.Level
import org.goldenport.config.Config
import org.goldenport.util.HoconUtils.RichConfig

/*
 * @since   Sep. 16, 2018
 * @version Sep. 19, 2018
 * @author  ASAMI, Tomoharu
 */
trait LispConfig extends EvalConfig {
  def defaultScriptLanguage = "javascript"
}

object LispConfig {
  val hocon = Config.loadHocon() // file
  val default = BasicLispConfig(hocon, None)
  val debug = BasicLispConfig(hocon, Some(Level.DEBUG))
  val trace = BasicLispConfig(hocon, Some(Level.TRACE))

  case class BasicLispConfig(
    hocon: RichConfig,
    logLevel: Option[Level]
  ) extends LispConfig {
  }
}
