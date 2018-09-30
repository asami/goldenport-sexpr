package org.goldenport.sexpr.eval

import org.goldenport.config.Config
import org.goldenport.sexpr._
import org.goldenport.io.MimeType
import org.goldenport.bag.ChunkBag
import org.goldenport.record.http.{Response, StringResponse, BinaryResponse}

/*
 * @since   Sep. 16, 2018
 * @version Sep. 18, 2018
 * @author  ASAMI, Tomoharu
 */
trait EvalConfig extends Config {
  val mimeSExprByString: Map[MimeType, String => SExpr] = Map(
    MimeType.text_html -> SHtml.apply,
    MimeType.text_xml -> SXml.apply,
    MimeType.application_xhtml_xml -> SHtml.apply,
    MimeType.application_json -> SJson.apply,
    MimeType.application_atom_xml -> SXml.apply,
    MimeType.application_rss_xml -> SXml.apply,
    MimeType.application_soap_xml -> SXml.apply
  )

  val mimeSExprByBinary: Map[MimeType, ChunkBag => SExpr] = Map()

  def toSExpr(p: Response): SExpr = {
    p match {
      case m: StringResponse =>
        println(s"toSExpr: ${m.mime}")
        println(s"toSExpr2: ${mimeSExprByString.get(m.mime)}")
      case m: BinaryResponse => ???
    }
    to_sexpr(p)
  }

  def to_sexpr(p: Response): SExpr = p match {
    case m: StringResponse => mimeSExprByString.get(m.mime).map(_.apply(m.content)).getOrElse(SString(m.content))
    case m: BinaryResponse => mimeSExprByBinary.get(m.mime).map(_.apply(m.content)).getOrElse(SBinary(m.content))
  }
}
