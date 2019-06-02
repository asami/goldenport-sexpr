package org.goldenport.sexpr.eval

import org.goldenport.RAISE
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.store._
import org.goldenport.sexpr._

/*
 * @since   Mar. 30, 2019
 * @version Mar. 31, 2019
 * @author  ASAMI, Tomoharu
 */
trait UtilityPart { self: LispFunction =>
  protected final def get_property_symbol(p: Parameters, key: Symbol): Option[Symbol] =
    p.getProperty(key).map {
      case SString(s) => Symbol(s)
      case SAtom(s) => Symbol(s)
      case m => RAISE.invalidArgumentFault(SError.invalidDatatype(key.name, m).print)
    }

  protected final def get_database(p: Parameters): Option[Symbol] = get_property_symbol(p, 'database)
}
