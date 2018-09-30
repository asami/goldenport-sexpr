package org.goldenport.sexpr.eval

import scala.util.parsing.input._
import org.goldenport.parser.{LogicalTokens, LogicalToken}

/*
 * @since   Sep.  2, 2018
 * @version Sep.  2, 2018
 * @author  ASAMI, Tomoharu
 */
case class LogicalTokensReader(
  tokens: LogicalTokens
) extends Reader[LogicalToken] {
  def first = tokens.head
  def rest = LogicalTokensReader(tokens.tail)
  def atEnd = tokens.isEmpty
  def pos =
    if (atEnd)
      NoPosition
    else
      first.location.
        map(x => new Position {
          def line = x.line getOrElse -1
          def column = x.offset getOrElse -1
          def lineContents = first.show
        }).getOrElse(NoPosition)
}
