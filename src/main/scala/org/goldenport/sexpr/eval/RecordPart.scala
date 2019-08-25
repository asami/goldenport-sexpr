package org.goldenport.sexpr.eval

import java.net.URI
import play.api.libs.json.JsValue
import org.goldenport.RAISE
import org.goldenport.record.v3.Record
import org.goldenport.sexpr._

/*
 * @since   Jul. 28, 2019
 * @version Jul. 30, 2019
 * @author  ASAMI, Tomoharu
 */
trait RecordPart { self: LispFunction =>
  protected final def record_make(u: LispContext, p: SExpr): SRecord = p match {
    case m: SRecord => m
    case m: SHtml => RAISE.invalidArgumentFault(s"no record: $m")
    case m: SXml => record_make(m.dom)
    case m: SJson => record_make(m.json)
    case m => RAISE.invalidArgumentFault(s"no record: $m")
  }

  protected final def record_make(p: org.w3c.dom.Node): SRecord = SRecord(Record.create(p))

  protected final def record_make(p: JsValue): SRecord = SRecord(Record.create(p))
}

object RecordPart {
}
