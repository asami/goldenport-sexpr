package org.goldenport.sexpr.eval

import javax.script._
import java.security._
import java.security.cert.Certificate
import org.goldenport.sexpr._

/*
 * @since   Sep. 18, 2018
 * @version Sep. 20, 2018
 * @author  ASAMI, Tomoharu
 */
trait ScriptEnginePart { self: LispContext =>
  // https://qiita.com/takaki@github/items/156818da334d53a47d92
  object script {
    def eval(p: SScript): LispContext = {
    val a = scriptContext.getEngineFactories()
//    println(s"S: $a")
    val lang = p.language getOrElse config.defaultScriptLanguage
      _eval(lang, p.text)
    }
  }

  private def _eval(name: String, script: String): LispContext =
    _get_engine(name).map(_eval(_, script)).getOrElse(toResult(SError(s"Unavailable script engine: $name")))

  private def _eval(engine: ScriptEngine, script: String): LispContext = {
    try {
      AccessController.doPrivileged(new PrivilegedAction[LispContext]() {
        def run() = {
          val r = engine.eval(script)
          val s = SExpr.create(r)
          toResult(s)
        }
      })
    } catch {
      case e: AccessControlException => toResult(SError(e))
      case e: PrivilegedActionException => toResult(SError(e))
    }
  }

  private def _get_engine(name: String): Option[ScriptEngine] =
    Option(scriptContext.getEngineByName(name))

  private def _access_control_context = {
    val permissions = new Permissions()
    val domain = new ProtectionDomain(
      new CodeSource(null, null: Array[Certificate]),
      permissions
    )
    new AccessControlContext(Array[ProtectionDomain](domain))
  }
}

// object ScriptEnginePart {
//   val manager = new ScriptEngineManager()
//   val engine = manager.getEngineByName("javascript")
//   if (engine == null)
//     ???

//   def f() = {
//   }
// }
