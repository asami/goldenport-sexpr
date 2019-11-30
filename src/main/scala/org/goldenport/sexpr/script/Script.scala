package org.goldenport.sexpr.script

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.log.Loggable
import org.goldenport.parser._
import org.goldenport.parser.XmlParser.XmlToken
import org.goldenport.parser.JsonParser.JsonToken
import org.goldenport.xsv.Lxsv
import org.goldenport.record.v3._
import org.goldenport.sexpr._

/*
 * @since   Sep.  3, 2018
 *  version Oct. 27, 2018
 *  version Jan.  3, 2019
 *  version Feb. 24, 2019
 *  version Mar.  9, 2019
 *  version May. 21, 2019
 *  version Jun. 30, 2019
 *  version Jul. 25, 2019
 *  version Aug. 31, 2019
 *  version Sep. 24, 2019
 *  version Oct. 31, 2019
 * @version Nov. 16, 2019
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
    // println(s"Script#parse: $p")
    val lc = LogicalLines.Config(useSingleQuote = false)
    val lines = LogicalLines.parse(lc, p)
    // println(s"Script#parse: $lines")
    lines.lines.foldMap(parse(config, _))
  }

  def parse(blocks: LogicalBlocks): Script = parse(Config.default, blocks)

  def parse(config: Config, blocks: LogicalBlocks): Script = {
    // println(s"Script#parse(blocks): $blocks")
    // println(s"Script#parse(blocks): $blocks.text")
    parse(config, blocks.text)
  }

  def parse(line: LogicalLine): Script = parse(Config.default, line)

  def parse(config: Config, line: LogicalLine): Script =
    if (line.text.startsWith(": ")) {
      val a = _parse(config, LogicalLine(line.text.substring(1).trim))
      a.expressions.headOption.
        map(x =>
          Script(SCell(SAtom(":"), x) +: a.expressions.tail)
        ).getOrElse(Script(SMetaCommand("")))
    } else if (line.text.startsWith(":")) {
      Script(SMetaCommand(line.text.substring(1)))
    } else {
      _parse(config, line)
    }

  private def _parse(config: Config, line: LogicalLine): Script = {
    val parser = LogicalTokenReaderWriterStateClass[Config, Script](config, ScriptState.init)
    val (messages, result, state) = parser.applySExpr(line.text)
    result match {
      case ParseSuccess(script, _) => script
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseDebug(p: String): Script = parse(Config.debug, p)

  case class Config(
    isDebug: Boolean = true,
    isLocation: Boolean = true
  ) extends ParseConfig {
    def isAutoFunction(name: String): Boolean = false
  }
  object Config {
    val default = Config()
    val debug = default.copy(isDebug = true)
  }

  trait ScriptParseState extends LogicalTokenReaderWriterState[Config, Script] with Loggable {
    def apply(config: Config, token: LogicalToken): Transition = {
      log_trace(s"SCRIPT PARSER IN ($this): $token")
      val r = handle_event(config, token)
      log_trace(s"SCRIPT PARSER OUT ($this): $token => $r")
      r
    }

    protected def handle_event(config: Config, token: LogicalToken): Transition =
      token match {
        case EndToken => handle_End(config)
        case m: AtomToken => handle_Atom(config, m)
        case m: ExpressionToken => handle_Expression(config, m)
        case m: DelimiterToken => handle_Delimiter(config, m)
        case m: SpaceToken => handle_Space(config, m)
        case m: SingleQuoteToken => handle_Single_Quote(config, m)
        case m: LiteralToken => handle_Literal(config, m)
        case m: EmptyToken => handle_Empty(config, m)
      }

    protected def handle_state_in_expression(config: Config, token: LogicalToken): ScriptParseState =
      token match {
        case m: AtomToken => add_Sexpr(config, atom_to_sexpr(m))
        case m: ExpressionToken => add_Sexpr(config, expression_to_sexpr(m))
        case _ => _handle_state_in_expression(config, token)
      }

    private def _handle_state_in_expression(config: Config, token: LogicalToken): ScriptParseState =
      handle_event(config, token) match {
        case (msgs, ParseSuccess(script, _), state) => RAISE.noReachDefect(getClass.getSimpleName)
        case (msgs, ParseFailure(_, _), state) => RAISE.noReachDefect(getClass.getSimpleName)
        case (msgs, EmptyParseResult(), state) => state
      }

    protected def handle_End(config: Config): Transition = RAISE.noReachDefect(getClass.getSimpleName)

    protected def handle_Atom(config: Config, t: AtomToken): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, atom_State(config, t))

    protected final def atom_to_sexpr(t: AtomToken): SExpr = {
      val name = t.name
      name.toLowerCase match {
        case "nil" => SNil
        case "t" => SBoolean.TRUE
        case "true" => SBoolean.TRUE
        case "false" => SBoolean.FALSE
        case  _ =>
          if (name.startsWith(":"))
            SKeyword(name.substring(1))
          else
            SAtom(name)
      }
    }

    protected def atom_State(config: Config, t: AtomToken): ScriptParseState =
      atom_to_sexpr(t) match {
        case m: SAtom => ExpressionCandidateState(this, Vector(m))
        case m => add_Sexpr(config, m)
      }

    protected def expression_to_sexpr(t: ExpressionToken): SExpr = SExpression(t.text)

    protected def handle_Expression(config: Config, t: ExpressionToken): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, expression_State(config, t))

    protected def expression_State(config: Config, t: ExpressionToken): ScriptParseState =
      ExpressionCandidateState(this, Vector(expression_to_sexpr(t)))

    protected def handle_Delimiter(config: Config, t: DelimiterToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, delimiter_State(config, t))

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState

    protected def handle_Space(config: Config, t: SpaceToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, space_State(config, t))

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState

    protected def handle_Single_Quote(config: Config, t: SingleQuoteToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, single_Quote_State(config, t))

    protected def single_Quote_State(config: Config, t: SingleQuoteToken): ScriptParseState =
      QuoteState(this)

    protected def handle_Literal(config: Config, t: LiteralToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, literal_State(config, t))

    protected def handle_Empty(config: Config, t: EmptyToken): Transition = 
      (ParseMessageSequence.empty, ParseResult.empty, this)

    protected def literal_State(config: Config, t: LiteralToken): ScriptParseState =
      t match {
        case m: AtomToken =>
          val name = m.name
          val a = name.toLowerCase match {
            case "nil" => SNil
            case "t" => SBoolean.TRUE
            case "true" => SBoolean.TRUE
            case "false" => SBoolean.FALSE
            case  _ => 
              if (name.startsWith(":"))
                SKeyword(name.substring(1))
              else
                SAtom(name)
          }
          add_Sexpr(config, a)
        case m: StringToken => add_Sexpr(config, _from_string(m))
        case m: BooleanToken => add_Sexpr(config, SBoolean(m.b))
        case m: NumberToken => add_Sexpr(config, SNumber(m.n))
        case m: RationalToken => add_Sexpr(config, SRational(m.n)) // SNumber
        case m: ComplexToken => add_Sexpr(config, SComplex(m.n))
        case m: RangeToken => add_Sexpr(config, SRange(m.range))
        case m: IntervalToken => add_Sexpr(config, SInterval(m.interval))
        case m: DateTimeToken => add_Sexpr(config, SDateTime(m.datetime))
        case m: LocalDateToken => add_Sexpr(config, SLocalDate(m.date))
        case m: LocalTimeToken => add_Sexpr(config, SLocalTime(m.time))
        case m: LocalDateTimeToken => add_Sexpr(config, SLocalDateTime(m.datetime))
        case m: MonthDayToken => add_Sexpr(config, SMonthDay(m.monthday))
        case m: PeriodToken => add_Sexpr(config, SPeriod(m.period))
        case m: DurationToken => add_Sexpr(config, SDuration(m.duration))
        case m: DateTimeIntervalToken => add_Sexpr(config, SDateTimeInterval(m.interval))
        case m: UrlToken => add_Sexpr(config, SUrl(m.url))
        case m: UriToken => add_Sexpr(config, SUri(m.uri))
        case m: UrnToken => add_Sexpr(config, SUrn(m.urn))
        case m: PathToken => add_Sexpr(config, SXPath(m.path))
        case m: ExpressionToken => add_Sexpr(config, SExpression(m.text))
        case m: ExplicitLiteralToken => RAISE.notImplementedYetDefect
        case m: ScriptToken => add_Sexpr(config, SScript.create(m))
        case m: XmlToken => add_Sexpr(config, SXml(m.text))
        case m: JsonToken => add_Sexpr(config, SJson(m.text))
        case m: BracketToken => add_Sexpr(config, _from_bracket(config, m))
        case m: DoubleBracketToken => add_Sexpr(config, SMatrix.create2d(m.prefix, m.text))
        case m: RawBracketToken => RAISE.unsupportedOperationFault(s"$m")
        case m: SingleQuoteToken => add_Sexpr(config, SSingleQuote()) // no reach
        case m: ExternalLogicalToken => RAISE.noReachDefect(getClass.getSimpleName)
        case m: XsvToken => add_Sexpr(config, SMatrix.create1d(None, m.text))
        case m: LxsvToken => add_Sexpr(config, SRecord.create(m.lxsv))
      }

    private def _from_string(p: StringToken) =
      p.prefix.map {
        case "s" => SList(SAtom("string-interpolate"), SString(p.text))
        // case "f" => SList(SAtom("string-format"), SString(p.text))
        case "record" => SRecord(_to_record(p.text))
        case "lxsv" => SLxsv(Lxsv.create(p.text))
        case "regex" => SRegex(new scala.util.matching.Regex(p.text))
        case "xml" => SXml(p.text)
        case "html" => SHtml(p.text)
        case "xpath" => SXPath(p.text)
        case "xsl" => SXsl(p.text)
        case "json" => SJson(p.text)
        case "pug" => SPug(p.text)
        // case "datetime" => SDateTime(p.text)
        // case "local-datetime" => SLocalDateTime(p.text)
        // case "local-date" => SLocalDate(p.text)
        // case "local-time" => SLocalTime(p.text)
        // case "monthday" => SMonthDay(p.text)
        // case "interval" => SInterval(p.text)
        // case "duration" => SDuration(p.text)
        // case "period" => SPeriod(p.text)
        // case "currency" => SCurrency(p.text)
        // case "percent" => SPercent(p.text)
        // case "unit" => SUnit(p.text)
        case _ => SString(p.text)
      }.getOrElse(SString(p.text))

    private def _to_record(p: String): IRecord =
      if (p.isEmpty)
        Record.empty
      else
        p(0) match {
          case '<' => _xml_to_record(p)
          case '{' => _json_to_record(p)
          case _ => _lxsv_to_record(p)
        }

    private def _xml_to_record(p: String): DomRecord = DomRecord.create(p)

    private def _json_to_record(p: String): JsonRecord = JsonRecord.create(p)

    private def _lxsv_to_record(p: String): Record = Record.fromLxsv(p)

    private def _from_bracket(config: Config, p: BracketToken): SVector = {
      val a = parse(config, p.text)
      val b = a.expressions.flatMap {
        case m: SList => m.vector
        case m => Vector(m)
      }
      SVector(b)
    }

    def addChildTransition(config: Config, ps: Vector[SExpr], t: LogicalToken): Transition = 
      add_Sexpr(config, ps).handle_event(config, t)

    def addChildState(config: Config, p: SExpr): ScriptParseState = add_Sexpr(config, p)

    def addChildState(config: Config, ps: Vector[SExpr]): ScriptParseState = add_Sexpr(config, ps)

    def addChildState(config: Config, ps: Vector[SExpr], t: LogicalToken): ScriptParseState =
      add_Sexpr(config, ps).handle_state_in_expression(config, t)

    protected def add_Sexpr(config: Config, p: SExpr): ScriptParseState = RAISE.noReachDefect(getClass.getSimpleName)

    protected def add_Sexpr(config: Config, ps: Vector[SExpr]): ScriptParseState =
      ps.toList match {
        case Nil => RAISE.noReachDefect(getClass.getSimpleName)
        case x :: Nil => add_Sexpr(config, x)
        case xs => RAISE.noReachDefect(getClass.getSimpleName)
      }

    protected final def empty_transition(state: ScriptParseState): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, state)
  }

  case class NormalState(
    parent: ScriptParseState,
    sexprs: Vector[SExpr] = Vector.empty
  ) extends ScriptParseState {
    override def handle_Atom(config: Config, t: AtomToken): Transition = {
      t.name match {
        case "'" => RAISE.notImplementedYetDefect
        case "." => _handle_association(config, t)
        case _ => super.handle_Atom(config, t)
      }
    }

    private def _handle_association(config: Config, t: AtomToken): Transition =
      if (sexprs.length != 1)
        RAISE.syntaxErrorFault(s"""Invalid association: (${sexprs.map(_.print).mkString(" ")} . ...""")
      else
        (ParseMessageSequence.empty, ParseResult.empty, StartAssociationState(parent, sexprs(0)))

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      t.s match {
        case "(" => NormalState(this)
        case ")" => parent.addChildState(config, SList.create(sexprs))
        case "," => this
        case s => RAISE.syntaxErrorFault(s"Unavailable delimiter '$s'")
      }

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      this

    override def add_Sexpr(config: Config, p: SExpr) = copy(sexprs = sexprs :+ p)

    override def add_Sexpr(config: Config, ps: Vector[SExpr]) = copy(sexprs = sexprs ++ ps)
  }
  object NormalState {
  }

  case class ScriptState(sexprs: Vector[SExpr]) extends ScriptParseState {
    override def add_Sexpr(config: Config, p: SExpr) = ScriptState(sexprs :+ p)

    override def add_Sexpr(config: Config, ps: Vector[SExpr]) = ScriptState(sexprs ++ ps)

    override protected def handle_End(config: Config): Transition = {
      val r = sexprs.toList match {
        case Nil => SNil
        case x :: Nil => x match {
          case m: SAtom if config.isAutoFunction(m.name) => SList(m)
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

  case class StartAssociationState(
    parent: ScriptParseState,
    car: SExpr
  ) extends ScriptParseState {
    override def add_Sexpr(config: Config, p: SExpr) = EndAssociationState(parent, car, p)

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      t.s match {
        case "(" => NormalState(this)
        case s => RAISE.syntaxErrorFault(s"Unavailable delimiter '$s'")
      }

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      this
  }

  case class EndAssociationState(
    parent: ScriptParseState,
    car: SExpr,
    cdr: SExpr
  ) extends ScriptParseState {
    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      t.s match {
        case ")" => parent.addChildState(config, SCell(car, cdr))
        case s => RAISE.syntaxErrorFault(s"Unavailable delimiter '$s'")
      }

    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      this
  }

  case class QuoteState(
    parent: ScriptParseState
  ) extends ScriptParseState {
    override def add_Sexpr(config: Config, p: SExpr) =
      parent.addChildState(config, SList(SAtom.quote, p))

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      t.s match {
        case "(" => NormalState(this)
        case s => RAISE.syntaxErrorFault(s"Unavailable delimiter '$s' after quote(').")
      }
    
    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      RAISE.syntaxErrorFault(s"Unavailable space after quote(').")
  }

  case class ExpressionCandidateState(
    parent: ScriptParseState,
    sexprs: Vector[SExpr]
  ) extends ScriptParseState {
    override protected def handle_event(config: Config, token: LogicalToken): Transition =
      token match {
        case EndToken => parent.addChildTransition(config, sexprs, EndToken)
        case m: DelimiterToken => m.s match {
          case "(" => empty_transition(ExpressionState(parent, sexprs, Vector(token)))
          case _ => parent.addChildTransition(config, sexprs, token)
        }
        case _ => parent.addChildTransition(config, sexprs, token)
      }

    // override protected def handle_End(config: Config): Transition =
    //   parent.addChildTransition(config, sexprs, EndToken)

    // protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
    //   t.s match {
    //     case "(" => ExpressionState(parent, sexprs, Vector(t))
    //     case s => parent.addChildState(config, sexprs, t)
    //   }

    // protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
    //   parent.addChildState(config, sexprs, t)

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      RAISE.noReachDefect
    
    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      RAISE.noReachDefect
  }

  case class ExpressionState(
    parent: ScriptParseState,
    sexprs: Vector[SExpr],
    tokens: Vector[LogicalToken],
    count: Int = 1
  ) extends ScriptParseState {
    override protected def handle_event(config: Config, token: LogicalToken): Transition =
      token match {
        case EndToken => parent.addChildTransition(config, Vector(_expression()), EndToken)
        case m: DelimiterToken => m.s match {
          case "(" => empty_transition(copy(tokens = tokens :+ token, count = count + 1))
          case ")" =>
            if (count <= 1)
              empty_transition(parent.addChildState(config, _expression(token)))
            else
              empty_transition(copy(tokens = tokens :+ token, count = count - 1))
          case _ => empty_transition(copy(tokens = tokens :+ token))
        }
        case _ => empty_transition(copy(tokens = tokens :+ token))
      }

    private def _text = sexprs.map(_.asString).mkString ++ tokens.map(_.raw).mkString

    private def _expression(): SExpression = SExpression(_text)

    private def _expression(p: LogicalToken): SExpression = SExpression(_text + p.raw)

    protected def delimiter_State(config: Config, t: DelimiterToken): ScriptParseState =
      RAISE.noReachDefect
    
    protected def space_State(config: Config, t: SpaceToken): ScriptParseState =
      RAISE.noReachDefect
  }
}
