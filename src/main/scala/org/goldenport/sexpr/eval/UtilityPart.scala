package org.goldenport.sexpr.eval

import org.goldenport.RAISE
import org.goldenport.record.v2.bag.CsvBag
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.store._
import org.goldenport.sexpr._

/*
 * @since   Mar. 30, 2019
 *  version Mar. 31, 2019
 *  version Jan. 26, 2020
 * @version Feb. 26, 2020
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

  // See LispContext#_csv_strategy
  protected final def csv_strategy(config: LispConfig) = CsvBag.Strategy.matrixAuto.update(
    Some(CsvBag.Strategy.matrixAuto.recordBagStrategy.update(
      config.getString("csv.codec").map(scalax.io.Codec.apply),
      None,
      None,
      None
    )),
    config.getString("csv.name"),
    config.getString("csv.lineEnd"),
    config.getBoolean("csv.isForceDoubleQuote")
  )
}
