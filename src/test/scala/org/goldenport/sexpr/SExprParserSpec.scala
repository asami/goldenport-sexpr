package org.goldenport.sexpr

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/*
 * @since   Sep.  9, 2012
 *  version Aug.  9, 2013
 *  version Feb.  4, 2014
 *  version Apr. 18, 2014
 *  version Sep. 14, 2014
 *  version Dec. 17, 2014
 *  version Feb.  6, 2015
 * @version Mar.  5, 2015
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class SExprParserSpec extends WordSpec with Matchers {
  "SExprParser" should {
    "parse" which {
      "empty parenthes" in {
        val s = SExprParser("()")
        s should equal (SNil)
      }
      "empty parenthes with space" in {
        val s = SExprParser("( )")
        s should be (SNil)
      }
      "nil" in {
        val s = SExprParser("nil")
        s should be (SNil)
      }
      "simple list" in {
        val s = SExprParser("(abc)")
        s should be (SCell(SAtom("abc"), SNil))
      }
      "日本語" in {
        val s = SExprParser("(日本語)")
        s should be (SCell(SAtom("日本語"), SNil))
      }
      "delimiter space" in {
        val s = SExprParser("(abc (xyz) )")
        s should be (SCell(SAtom("abc"), SCell(SCell(SAtom("xyz"), SNil), SNil)))
      }
    }
  }

  "Comma Separated SExpr" should {
    "parse" which {
      "simple list" in {
        val s = SExprParser("(abc,xyz)")
        s should be (SCell(SAtom("abc"), SCell(SAtom("xyz"), SNil)))
      }
    }
  }

  "Comment" should {
    "typical" in {
      val s = SExprParser(";;comment\n(abc,xyz)")
      s should be (SCell(SAtom("abc"), SCell(SAtom("xyz"), SNil)))
    }
  }

  "String literal" should {
    "typical" in {
      val s = SExprParser("\"abc\"")
      s should be (SString("abc"))
    }
    "newline" in {
      val s = SExprParser("\"abc\\ndef\\n\"")
      s should be (SString("abc\ndef\n"))
    }
    "double quote" in {
      val s = SExprParser("\"abc\\nd\\\"e\\\"f\\n\"")
      s should be (SString("abc\nd\"e\"f\n"))
      val t = s.toString
      t should be ("\"abc\\nd\\\"e\\\"f\\n\"")
      s.asInstanceOf[SString].string should be ("abc\nd\"e\"f\n")
    }
  }

  "Row string literal" should {
    "typical" in {
      val s = SExprParser("\"\"\"abc\ndef\n\"\"\"")
      s should be (SString("abc\ndef\n"))
    }
    "doule quote" in {
      val s = SExprParser("\"\"\"abc\nd\"e\"f\n\"\"\"")
      s should be (SString("abc\nd\"e\"f\n"))
    }
  }

  "Real world" should {
    "Long string" in {
      val expr = """("FREAKSNEWShttp://www.freaksstore.com${full_name:フリークスストア会員}様現在の会員ステージ：${current_rank:}只今のポイント：${current_point:}ポイントポイントの有効期限：${point_expired_at:}いつもFREAKSSTOREをご愛顧頂きまして誠にありがとうございます。。今週は注目のアイテム、ニューバランスとトレンドの店舗限定のインディアンジュエリーのご紹介です！【WOMENS】今の気分は、春らしいnewbalnce。次の１足にはこちらがオススメ!ニューバランスの春夏スタート！http://freaksstore.com/news/2015/02/new-balnce.php【MENS】伝統と革新が織りなす日本初上陸のインディアンジュエリー。http://freaksstore.com/news/2015/02/mens-4.php3.20(金)NEWSHOP、なんばパークス店GRANDOPEN!!http://freaksstore.com/news/2015/02/320new-shopgrand-open.php" (html "<metahttp-equiv=Content-Languagecontent=ja><metahttp-equiv=Content-Typecontent=text/html;charset=iso-2022-jp><title></title><metahttp-equiv=Content-Style-Typecontent=text/css><styletype=text/css><!--body{line-height:22px;color:#000000;width:600px;margin:0auto;text-align:center;font-size:12px;}tabletrtd{line-height:1.6em;}img{vertical-align:middle;}a:link{color:#61852d;}--></style><styletype=text/css><!--p.MsoPlainText{margin:0mm;margin-bottom:.0001pt;font-size:10.0pt;font-family:ＭＳゴシック;}--></style><styletype=text/css><!--p.MsoNormal{margin:0mm;margin-bottom:.0001pt;font-size:12.0pt;font-family:ＭＳＰゴシック;}--></style><divstyle=width:600px;margin:0auto;padding:10px030px;background:#fff;><tablecellpad<metahttp-equiv=Content-Languagecontent=ja><metahttp-equiv=Content-Typecontent=text/html;charset=iso-2022-jp><title></title><metahttp-equiv=Content-Style-Typecontent=text/css><styletype=text/css><!--body{line-height:22px;color:#000000;width:600px;margin:0auto;text-align:center;font-size:12px;}tabletrtd{line-height:1.6em;}img{vertical-align:middle;}a:link{color:#61852d;}--></style><styletype=text/css><!--p.MsoPlainText{margin:0mm;margin-bottom:.0001pt;font-size:10.0pt;font-family:ＭＳゴシック;}--></style><styletype=text/css><!--p.MsoNormal{margin:0mm;margin-bottom:.0001pt;font-size:12.0pt;font-family:ＭＳＰゴシック;}--></style><divstyle=width:600px;margin:0auto;padding:10px030px;background:#fff;><tablecellpad<metahttp-equiv=Content-Languagecontent=ja><metahttp-equiv=Content-Typecontent=text/html;charset=iso-2022-jp><title></title><metahttp-equiv=Content-Style-Typecontent=text/css><styletype=text/css><!--body{line-height:22px;color:#000000;width:600px;margin:0auto;text-align:center;font-size:12px;}tabletrtd{line-height:1.6em;}img{vertical-align:middle;}a:link{color:#61852d;}--></style><styletype=text/css><!--p.MsoPlainText{margin:0mm;margin-bottom:.0001pt;font-size:10.0pt;font-family:ＭＳゴシック;}--></style><styletype=text/css><!--p.MsoNormal{margin:0mm;margin-bottom:.0001pt;font-size:12.0pt;font-family:ＭＳＰゴシック;}--></style><divstyle=width:600px;margin:0auto;padding:10px030px;background:#fff;><tablecellpad<metahttp-equiv=Content-Languagecontent=ja><metahttp-equiv=Content-Typecontent=text/html;charset=iso-2022-jp><title></title><metahttp-equiv=Content-Style-Typecontent=text/css><styletype=text/css><!--body{line-height:22px;color:#000000;width:600px;margin:0auto;text-align:center;font-size:12px;}tabletrtd{line-height:1.6em;}img{vertical-align:middle;}a:link{color:#61852d;}--></style><styletype=text/css><!--p.MsoPlainText{margin:0mm;margin-bottom:.0001pt;font-size:10.0pt;font-family:ＭＳゴシック;}--></style><styletype=text/css><!--p.MsoNormal{margin:0mm;margin-bottom:.0001pt;font-size:12.0pt;font-family:ＭＳＰゴシック;}--></style><divstyle=width:600px;margin:0auto;padding:10px030px;background:#fff;><tablecellpad<metahttp-equiv=Content-Languagecontent=ja><metahttp-equiv=Content-Typecontent=text/html;charset=iso-2022-jp><title></title><metahttp-equiv=Content-Style-Typecontent=text/css><styletype=text/css><!--body{line-height:22px;color:#000000;width:600px;margin:0auto;text-align:center;font-size:12px;}tabletrtd{line-height:1.6em;}img{vertical-align:middle;}a:link{color:#61852d;}--></style><styletype=text/css><!--p.MsoPlainText{margin:0mm;margin-bottom:.0001pt;font-size:10.0pt;font-family:ＭＳゴシック;}--></style><styletype=text/css><!--p.MsoNormal{margin:0mm;margin-bottom:.0001pt;font-size:12.0pt;font-family:ＭＳＰゴシック;}--></style><divstyle=width:600px;margin:0auto;padding:10px030px;background:#fff;><tablecellpad"))"""
      val s = SExprParser(expr)
    }
  }
}
