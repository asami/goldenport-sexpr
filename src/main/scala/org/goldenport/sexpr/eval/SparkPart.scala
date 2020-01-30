package org.goldenport.sexpr.eval

import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v3.ITable
import org.goldenport.util.StringUtils
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.spark.SparkContext

/*
 * @since   Jan. 19, 2020
 * @version Jan. 19, 2020
 * @author  ASAMI, Tomoharu
 */
trait SparkPart { self: LispContext =>
  private lazy val _spark_context = new SparkContext(this)

  object spark {
    def createDataFrame(p: STable): SDataFrame = createDataFrame(p.table)

    def createDataFrame(p: ITable): SDataFrame = {
      val a = _spark_context.createDataFrame(p)
      SDataFrame(a)
    }

    // def select(df: SDataFrame, names: Seq[String]): SDataFrame = ???
  }
}
