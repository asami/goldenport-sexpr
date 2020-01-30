package org.goldenport.sexpr.eval.spark

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import scala.collection.JavaConverters._
import org.apache.spark.SparkConf
import org.apache.spark.sql.{Column => SColumn, _}
import org.apache.spark.sql.types._
import org.goldenport.RAISE
import org.goldenport.record.v2.{Schema, Column => RColumn, MOne,MZeroOne, MOneMore, MZeroMore}
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.v3.ITable
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Jan. 19, 2020
 * @version Jan. 19, 2020
 * @author  ASAMI, Tomoharu
 */
class SparkContext(context: LispContext) {
//  val conf = new SparkConf()
  val session = SparkSession.
    builder().
//    config(conf).
    appName("Spark SQL"). // TODO
    master("local").
    config("spark.some.config.option", "some-value"). // TODO
    getOrCreate()

  def createDataFrame(p: ITable) = {
    val rows: Seq[Row] = p.toTable.toVectorVector.map(_to_row)
    val schema: StructType = _to_struct_type(p.schema)
    new SparkDataFrame(this, session.createDataFrame(rows.asJava, schema))
  }

  private def _to_row(p: Vector[Any]): Row = Row.fromSeq(p)

  private def _to_struct_type(p: Schema): StructType =
    StructType(p.columns.map(_to_column))

  private def _to_column(p: RColumn) = {
    val t = p.datatype match {
      case _ => StringType
    }
    val nullable = p.multiplicity match {
      case MOne => false
      case MZeroOne => true
      case MOneMore => false
      case MZeroMore => true
      case _ => false
    }
    StructField(p.name, t, nullable)
  }

  // def select(df: DataFrame, names: Seq[String]): SparkDataFrame = ???
}
