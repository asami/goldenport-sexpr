package org.goldenport.sexpr.eval

import org.goldenport.config.Config
import org.goldenport.log.LogLevel
import org.goldenport.hocon.RichConfig

/*
 * @since   Sep. 16, 2018
 *  version Oct.  6, 2018
 * @version Mar. 24, 2019
 * @author  ASAMI, Tomoharu
 */
trait LispConfig extends EvalConfig {
  def defaultScriptLanguage = "javascript"
}

object LispConfig {
  val properties = Config.loadHocon() // file
  val default = BasicLispConfig(LogLevel.Info, properties)
  val debug = BasicLispConfig(LogLevel.Debug, properties)
  val trace = BasicLispConfig(LogLevel.Trace, properties)

  case class BasicLispConfig(
    logLevel: LogLevel,
    properties: RichConfig
  ) extends LispConfig {
  }
}
