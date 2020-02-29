package org.goldenport.sexpr.eval

import javax.script._
import java.security._
import java.security.cert.Certificate
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v2.util.BeanUtils
import org.goldenport.util.StringUtils
import org.goldenport.sexpr._

/*
 * @since   Sep. 18, 2018
 *  version Aug. 31, 2019
 *  version Sep. 29, 2019
 *  version Nov. 29, 2019
 * @version Feb. 29, 2020
 * @author  ASAMI, Tomoharu
 */
trait ScriptEnginePart { self: LispContext =>
  import ScriptEnginePart._

  // https://qiita.com/takaki@github/items/156818da334d53a47d92
  object script {
    def eval(p: SScript): LispContext = {
      // val a = scriptContext.getEngineFactories()
      // println(s"S: $a")
      val lang = p.language getOrElse config.defaultScriptLanguage
      _eval(lang, p.text)
    }

    def eval(p: SExpression): LispContext =
      _eval_simple_expression_option(p) getOrElse {
        val lang = config.defaultExpressionLanguage
        _eval_format(lang, p.expression)
      }
  }

  private def _eval(name: String, script: String): LispContext =
    _get_engine(name).map(_eval(_, script)).getOrElse(toResult(SError(s"Unavailable script engine: $name")))

  private def _eval(engine: ScriptEngine, script: String): LispContext = {
    try {
      AccessController.doPrivileged(new PrivilegedAction[LispContext]() {
        def run() = try {
          val r = engine.eval(script)
          val s = SExpr.create(r)
          toResult(s)
        } catch {
          case e: ScriptException => toResult(SError(e))
        }
      })
    } catch {
      case e: AccessControlException => toResult(SError(e))
      case e: PrivilegedActionException => toResult(SError(e))
    }
  }

  private def _get_engine(name: String): Option[ScriptEngine] =
    scriptContext.createEngineOption(name, bindings)

  private def _access_control_context = {
    val permissions = new Permissions()
    val domain = new ProtectionDomain(
      new CodeSource(null, null: Array[Certificate]),
      permissions
    )
    new AccessControlContext(Array[ProtectionDomain](domain))
  }

  private def _eval_simple_expression_option(p: SExpression): Option[LispContext] = {
    val s = if (p.expression.startsWith("."))
      "?" :: Strings.totokens(p.expression, ".")
    else
      Strings.totokens(p.expression, ".")
    _eval_simple_expression_option(s)
  }

  private def _eval_simple_expression_option(ps: List[String]): Option[LispContext] =
    if (_is_simple_expression(ps))
      _eval_simple_expression(ps)
    else
      None

  private def _is_simple_expression(ps: List[String]) = {
    val xs = ps match {
      case Nil => Nil
      case xs => xs.init :+ xs.last.span(_ != '%')._1
    }
    xs match {
      case Nil => false
      case x :: xs => _is_first_component(x) && xs.forall(StringUtils.isScriptIdentifier)
    }
  }

  private def _is_first_component(p: String) = p match {
    case "#" => true
    case "?" => true
    case "!" => true
    case _ =>
      StringUtils.getMarkInt(p).collect {
        case ("#", _) => true
        case ("?", _) => true
        case ("!", _) => true
      }.getOrElse(false) || StringUtils.isScriptIdentifier(p)
  }

  private def _eval_simple_expression(ps: List[String]): Option[LispContext] = ps match {
    case Nil => RAISE.noReachDefect
    case x :: Nil =>
      val (expr, format) = _parse_format(x)
      _get_property(expr).map(x => toResult(_format(format, SExpr.create(x))))
    // case xs =>
    //   val key = xs.init.mkString(".") // TODO
    //   val (propertyname, format) = _parse_format(xs.last)
    //   bindings.get(key).flatMap(bean =>
    //     bean match {
    //       case m: AnyRef => BeanUtils.getProperty(m, propertyname).
    //           map(x => toResult(_format(format, SExpr.create(x))))
    //       case _ => None
    //     }
    //   )
    case xs =>
      val (key, format) = _parse_format(xs.mkString("."))
      bindings.get(key).
        map { x =>
          toResult(_format(format, SExpr.create(x)))
        }.orElse (_eval_simple_expression(xs.init, List(xs.last)))
  }

  private def _eval_simple_expression(property: List[String], path: List[String]): Option[LispContext] =
    property match {
      case Nil => None
      case xs => 
        val key = xs.mkString(".")
        val v = _get_property(key)
        v.flatMap {
          case m: AnyRef => _eval_bean(m, path)
          case _ => None
        }.orElse(_eval_simple_expression(property.init, property.last :: path))
    }

  // See org.goldenport.kaleidox.lisp.Evaluator.normalize.
  private def _get_property(key: String): Option[Any] = key match {
    case "#" => Some(takeHistory)
    case "?" => Some(peek)
    case "!" => Some(takeCommandHistory)
    case _ => StringUtils.getMarkInt(key).collect {
      case ("#", i) => takeHistory(i)
      case ("?", i) => peek(i)
      case ("!", i) => takeCommandHistory(i)
    }.orElse(
      bindings.get(key)
    )
  }

  private def _eval_bean(o: Any, path: List[String]): Option[LispContext] =
    o match {
      case m: AnyRef => path match {
        case Nil => RAISE.noReachDefect
        case x :: Nil =>
          val (propertyname, format) = _parse_format(x)
          _get_property(m, propertyname).
            map(v => toResult(_format(format, SExpr.create(v))))
        case x :: xs => _get_property(m, x).flatMap(v => _eval_bean(v, xs))
      }
      case _ => None
    }

  private def _get_property(o: AnyRef, propertyname: String): Option[Any] = o match {
    case m: SRecord => m.record.get(propertyname)
    // See ScriptEngineContext#_value
    case m: SExpr => BeanUtils.getProperty(m, propertyname) // orElse _get_property(mm.asJavaObject, propertyname, format)
    case m => BeanUtils.getProperty(m, propertyname)
  }

  private def _eval_format(name: String, script: String): LispContext = {
    val (s, format) = _parse_format(script)
    val ctx = _eval(name, s)
    ctx.toResult(_format(format, ctx.value))
  }

  private def _parse_format(p: String): (String, Option[String]) = {
    val (s, post) = p.span(_ != '%')
    post.length match {
      case 0 => (p, None)
      case 1 => (p, None)
      case _ => (s, Some(post.substring(1)))
    }
  }

  private def _format(fmt: Option[String], p: SExpr): SExpr =
    fmt.map(format(_, p)).getOrElse(p)
}

object ScriptEnginePart {
//  val firstComponentRegex = """^[#?!]\d+[.]*""".r
}
