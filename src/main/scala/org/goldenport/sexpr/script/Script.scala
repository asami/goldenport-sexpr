package org.goldenport.sexpr.script

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE
import org.goldenport.parser._
import org.goldenport.parser.XmlParser.XmlToken
import org.goldenport.parser.JsonParser.JsonToken
import org.goldenport.sexpr._

/*
 * @since   Sep.  3, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class Script(expressions: Vector[SExpr]) {
  def +(rhs: Script) = Script(expressions ++ rhs.expressions)
}

object Script {
  type Transition = (ParseMessageSequence, ParseResult[Script], ScriptParseState)

  val empty = Script(Vector.empty)

  implicit object ScriptMonoid extends Monoid[Script] {
    def zero = Script.empty
    def append(lhs: Script, rhs: => Script) = lhs + rhs
  }

  def apply(p: SExpr, ps: SExpr*): Script =
    Script(p +: ps.toVector)

  def parse(p: String): Script = parse(Config.default, p)

  def parse(config: Config, p: String): Script = {
    val lines = LogicalLines.parse(p)
    lines.lines.foldMap(parse(config, _))
  }

  def parse(blocks: LogicalBlocks): Script = parse(Config.default, blocks)

  def parse(config: Config, blocks: LogicalBlocks): Script =
    blocks.lines.lines.map(parse).concatenate

  def parse(line: LogicalLine): Script = parse(Config.default, line)

  def parse(config: Config, line: LogicalLine): Script = {
    val parser = LogicalTokenReaderWriterStateClass[Config, Script](config, ScriptState.init)
    val (messages, result, state) = parser.apply(line.text)
    result match {
      case ParseSuccess(script, _) => script
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseDebug(p: String): Script = parse(Config.debug, p)

  case class Config(
    isDebug: Boolean = true
  ) extends ParseConfig {
  }
  object Config {
    val default = Config()
    val debug = default.copy(isDebug = true)
  }

  trait ScriptParseState extends LogicalTokenReaderWriterState[Config, Script] {
    def apply(config: Config, token: LogicalToken): Transition = {
      if (config.isDebug)
        println(s"IN ($this): $token") // TODO slf4j
      val r = handle_event(config, token)
      if (config.isDebug)
        println(s"OUT ($this): $r") // TODO slf4j
      r
    }

    protected final def handle_event(config: Config, token: LogicalToken): Transition =
      token match {
        case EndToken => handle_End(config)
        case m: AtomToken => handle_Atom(config, m)
        case m: DelimiterToken => handle_Delimiter(config, m)
        case m: SpaceToken => handle_Space(config, m)
        case m: LiteralToken => handle_Literal(config, m)
      }

    protected def handle_End(config: Config): Transition = RAISE.noReachDefect(getClass.getSimpleName)

    protected def handle_Atom(config: Config, t: AtomToken): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, atom_State(config, t))

    protected def atom_State(config: Config, t: AtomToken): ScriptParseState =
      literal_State(config, t)

    protected def handle_Delimiter(config: Config, t: DelimiterToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, delimiter_State(config, t))

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState

    protected def handle_Space(config: Config, t: SpaceToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, space_State(config, t))

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState

    protected def handle_Literal(config: Config, t: LiteralToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, literal_State(config, t))

    protected def literal_State(config: Config, t: LiteralToken): ScriptParseState =
      t match {
        case m: AtomToken => add_Sexpr(SAtom(m.name))
        case m: StringToken => add_Sexpr(_to_string(m))
        case m: BooleanToken => add_Sexpr(SBoolean(m.b))
        case m: NumberToken => add_Sexpr(SNumber(m.n))
        case m: DateTimeToken => add_Sexpr(SDateTime(m.datetime))
        case m: LocalDateToken => add_Sexpr(SLocalDate(m.date))
        case m: LocalTimeToken => add_Sexpr(SLocalTime(m.time))
        case m: UrlToken => add_Sexpr(SUrl(m.url))
        case m: UrnToken => add_Sexpr(SUrn(m.urn))
        case m: PathToken => add_Sexpr(SXPath(m.path))
        case m: XmlToken => add_Sexpr(SXml(m.text))
        case m: JsonToken => add_Sexpr(SJson(m.text))
        case m: BracketToken => add_Sexpr(SScript(m.prefix, m.text))
        case m: RawBracketToken => add_Sexpr(SScript(m.prefix, m.text))
        case m: ExternalLogicalToken => RAISE.noReachDefect(getClass.getSimpleName)
        case _: DelimiterToken => RAISE.noReachDefect(getClass.getSimpleName)
      }

    private def _to_string(p: StringToken) = {
      p.prefix.map {
        case "regex" => SRegex(new scala.util.matching.Regex(p.text))
        case "xpath" => SXPath(p.text)
        case "xslt" => SXslt(p.text)
        case "pug" => SPug(p.text)
        case _ => SString(p.text)
      }.getOrElse(SString(p.text))
        
    }

    def addChildState(config: Config, p: SExpr) = add_Sexpr(p)

    protected def add_Sexpr(p: SExpr): ScriptParseState = RAISE.noReachDefect(getClass.getSimpleName)
  }

  case class NormalState(
    parent: ScriptParseState,
    sexprs: Vector[SExpr] = Vector.empty
  ) extends ScriptParseState {
    override def add_Sexpr(p: SExpr) = copy(sexprs = sexprs :+ p)

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      t.s match {
        case "(" => NormalState(this)
        case ")" => parent.addChildState(config, SList.create(sexprs))
        case "," => this
        case s => RAISE.syntaxErrorFault(s"Unavailable delimiter '$s'")
      }

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      this
  }
  object NormalState {
  }

  case class ScriptState(sexprs: Vector[SExpr]) extends ScriptParseState {
    override def add_Sexpr(p: SExpr) = ScriptState(sexprs :+ p)

    override protected def handle_End(config: Config): Transition = {
      val r = sexprs.toList match {
        case Nil => SNil
        case x :: Nil => x match {
          case m: SAtom => SList(m)
          case m => m
        }
        case xs => SList.create(xs)
      }
      (ParseMessageSequence.empty, ParseSuccess(Script(r)), ScriptState.init)
    }

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      t.s match {
        case "(" => NormalState(this)
        case ")" => RAISE.syntaxErrorFault("Unavailable delimiter ')'")
        case "," => this
        case s => RAISE.syntaxErrorFault(s"Unavailable delimiter '$s'")
      }

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      this

    // override protected def literal_State(config: Config, t: LiteralToken): ScriptParseState =
    //   t match {
    //     case m: UrlToken => add_Sexpr(SList(SAtom("fetch"), SUrl(m.url)))  // TODO Kaleidox
    //     case AtomToken(name, _) if name == "+" => add_Sexpr(SList(SAtom("+"), SList(SAtom("pop")), SList(SAtom("pop"))))
    //     case _ => super.literal_State(config, t)
    //   }
  }
  object ScriptState {
    val init = ScriptState(Vector.empty)
  }
}
