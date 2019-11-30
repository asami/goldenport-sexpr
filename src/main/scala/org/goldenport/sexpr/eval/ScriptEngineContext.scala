package org.goldenport.sexpr.eval

import scala.collection.JavaConverters._
import javax.script._
import org.apache.commons.jexl3.scripting.JexlScriptEngineFactory
import org.goldenport.record.v3.IRecord
import org.goldenport.python.JepPythonEngineFactory
import org.goldenport.sexpr._

/*
 * @since   Sep.  1, 2019
 *  version Sep. 29, 2019
 * @version Nov. 28, 2019
 * @author  ASAMI, Tomoharu
 */
case class ScriptEngineContext(
) {
  lazy val scriptEngineManager = {
    val r = new ScriptEngineManager()
    _setup_jexl(r)
    _setup_python(r)
    r
  }

  private def _setup_jexl(p: ScriptEngineManager) {
    val factory = new JexlScriptEngineFactory()
    // TODO log4j for factory
    p.registerEngineName("jexl", factory)
  }

  private def _setup_python(p: ScriptEngineManager) {
    val factory = new JepPythonEngineFactory()
    factory.getNames.asScala.foreach(p.registerEngineName(_, factory))
    factory.getMimeTypes.asScala.foreach(p.registerEngineMimeType(_, factory))
    factory.getExtensions.asScala.foreach(p.registerEngineExtension(_, factory))
  }

  def createEngineOption(name: String, bindings: IRecord): Option[ScriptEngine] =
    createEngineOption(name, bindings.toMap)

  def createEngineOption(name: String, bindings: Map[String, Any]): Option[ScriptEngine] =
    Option(scriptEngineManager.getEngineByName(name)).map { x =>
      bindings foreach {
        case (k, v) => x.put(k, _value(v))
      }
      x
    }

  // See ScriptEnginePart#_eval_bean
  private def _value(p: Any): Any = p match {
    case m: SBoolean => m.value
    case m: SNumber => m.asBigDecimal
    case m: SString => m.asString
    case m: SMatrix => m
    case m: SRecord => m
    case m: STable => m
    case m: SExpr => m.asObject
    case m => m
  }
}

object ScriptEngineContext {
  def default = ScriptEngineContext()
}
