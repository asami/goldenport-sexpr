package org.goldenport.sexpr.eval.spark

import scalaz.{Store => _, Id => _, _}, Scalaz.{Id => _, _}
import org.apache.spark.sql.SparkSession
import org.goldenport.RAISE
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.{Record, RecordSequence}
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Apr. 16, 2019
 * @version Jan. 19, 2020
 * @author  ASAMI, Tomoharu
 */
object SparkFunction {
  val functions = Vector(
    SparkDataFrame
    // SparkGet, SparkQuery, SparkInsert, SparkUpdate, SparkDelete,
    // SparkCreate, SparkDrop
  )

  case object SparkDataFrame extends IoFunction {
    val specification = FunctionSpecification("dataframe", 2)

    def apply(p: LispContext): LispContext = {
      val r = p.parameters.head match {
        case m: STable => p.spark.createDataFrame(m)
        case m => RAISE.syntaxErrorFault(s"Invalid for dataframe: ${m}")
      }
      p.toResult(r)
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }

  // private def x = {
  //   val spark = SparkSession
  //     .builder()
  //     .appName("Spark SQL basic example")
  //     .config("spark.some.config.option", "some-value")
  //     .getOrCreate()

  //   import spark.implicits._

  //   // val s: String = spark.read
  //   // val s: String = spark.create
  //   ???
  // }

  // case object SparkGet extends IoFunction {
  //   val specification = FunctionSpecification("spark-get", 2)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.sparkCollection
  //       id <- p.param.idForSpark
  //     } yield {
  //       (collection |@| id)(p.feature.spark.get(_, _)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  // case object SparkQuery extends IoFunction {
  //   val specification = FunctionSpecification("spark-query", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.sparkCollection
  //       query <- p.param.query
  //     } yield {
  //       (collection |@| query)(p.feature.spark.query(_, _)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  // case object SparkInsert extends IoFunction {
  //   val specification = FunctionSpecification("spark-insert", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.sparkCollection
  //       rec <- p.param.record
  //     } yield {
  //       (collection |@| rec)(p.feature.spark.insert(_, _)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  // case object SparkUpdate extends IoFunction {
  //   val specification = FunctionSpecification("spark-update", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.sparkCollection
  //       id <- p.param.idForSpark
  //       rec <- p.param.record
  //     } yield {
  //       (collection |@| id |@| rec)(p.feature.spark.update(_, _, _)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  // case object SparkDelete extends IoFunction {
  //   val specification = FunctionSpecification("spark-delete", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.sparkCollection
  //       id <- p.param.idForSpark
  //     } yield {
  //       (collection |@| id)(p.feature.spark.delete(_, _)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  // case object SparkCreate extends IoFunction {
  //   val specification = FunctionSpecification("spark-create", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.argument1[Symbol]
  //       schema <- p.param.argument1[Schema]
  //     } yield {
  //       (collection |@| schema)(p.feature.spark.create(_, _)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }

  // case object SparkDrop extends IoFunction {
  //   val specification = FunctionSpecification("spark-drop", 1)

  //   def apply(p: LispContext): LispContext = {
  //     val a = for {
  //       collection <- p.param.sparkCollection
  //       id <- p.param.idForSpark
  //     } yield {
  //       collection.map(p.feature.spark.drop(_)).valueOr(SError(_))
  //     }
  //     val r = a.run(p.param.cursor(specification))
  //     p.toResult(r._2)
  //   }

  //   def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  // }
}
