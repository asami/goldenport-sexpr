package org.goldenport.sexpr.eval.spark

import org.apache.spark.sql.DataFrame
import org.goldenport.RAISE
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.{IRecord, Record, ITable, RecordSequence}
import org.goldenport.matrix.IMatrix
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Jan.  6, 2020
 * @version Jan. 19, 2020
 * @author  ASAMI, Tomoharu
 */
class SparkDataFrame(context: SparkContext, dataframe: DataFrame) extends IDataFrame {
}

object SparkDataFrame {
  // def create(p: ITable): SparkDataFrame = create(p.schema, p.toRecordVector)

  // def create(s: Schema, rs: Seq[Record]): SparkDataFrame = {
  //   RAISE.notImplementedYetDefect
  // }

  // def create(s: Schema, rs: RecordSequence): SparkDataFrame = {
  //   RAISE.notImplementedYetDefect
  // }

  // def create(rs: Seq[Record]): SparkDataFrame = {
  //   RAISE.notImplementedYetDefect
  // }

  // def create(rs: RecordSequence): SparkDataFrame = {
  //   RAISE.notImplementedYetDefect
  // }

  // def create(p: IMatrix[_]): SparkDataFrame = {
  //   RAISE.notImplementedYetDefect
  // }
}
