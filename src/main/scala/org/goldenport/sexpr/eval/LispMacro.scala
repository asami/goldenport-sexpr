package org.goldenport.sexpr.eval

import org.goldenport.sexpr._, SExprConverter._

/*
 * @since   Mar.  3, 2019
 * @version Mar.  3, 2019
 * @author  ASAMI, Tomoharu
 */
trait LispMacro extends PartialFunction[LispContext, LispContext] {
}
