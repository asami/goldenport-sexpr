package org.goldenport.sexpr.eval.sql

import org.goldenport.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._

/*
 * @since   Mar. 23, 2019
 *  version Mar. 24, 2019
 *  version Apr.  8, 2019
 * @version Oct.  3, 2019
 * @author  ASAMI, Tomoharu
 */
object SqlFunction {
  val functions = Vector(Sql)

  case object Sql extends IoFunction {
    val specification = FunctionSpecification("sql", 1)

    def apply(p: LispContext): LispContext = {
      val db = p.parameters.getProperty('database).map {
        case SString(s) => Symbol(s)
        case SAtom(s) => Symbol(s)
        case m => RAISE.invalidArgumentFault(SError.invalidDatatype("database", m).print)
      }
      val r = p.parameters.argument(1) match {
        case SString(s) =>
          val x = s.take(6).toLowerCase
          if (_is_access(x))
            _query(p.sqlContext, db, s)
          else if (_is_mutate(x))
            _mutate(p.sqlContext, db, s)
          else
            _execute(p.sqlContext, db, s)

        case m => SError(s"Not sql: ${m.show}")
      }
      p.toResult(r)
    }

    private val _access_keywords = Vector(
      "select"
    )

    private val _access_keywords_length: Int = _access_keywords.map(_.length).max

    private val _mutation_keywords = Vector(
      "insert",
      "update",
      "delete" // ,
      // "create",
      // "drop",
      // "alter"
    )
    private val _mutation_keywords_length: Int = _mutation_keywords.map(_.length).max

    private def _is_access(p: String) = {
      val x = p.take(_access_keywords_length).toLowerCase
      _access_keywords.contains(x)
    }

    private def _is_mutate(p: String) = {
      val x = p.take(_mutation_keywords_length).toLowerCase
      _mutation_keywords.contains(x)
    }

    private def _query(ctx: SqlContext, db: Option[Symbol], s: String) = {
      val rs = db.map(ctx.select(_, s)).getOrElse(ctx.select(s))
      SList.create(rs.map(SRecord.apply))
    }

    private def _mutate(ctx: SqlContext, db: Option[Symbol], s: String) = {
      val n = db.map(ctx.mutate(_, s)).getOrElse(ctx.mutate(s))
      SNumber(n)
    }

    private def _execute(ctx: SqlContext, db: Option[Symbol], s: String) = {
      db.map(ctx.execute(_, s)).getOrElse(ctx.mutate(s))
      SBoolean.TRUE
    }

    def applyEffect(p: LispContext): UnitOfWorkFM[LispContext] = RAISE.notImplementedYetDefect
  }
}
