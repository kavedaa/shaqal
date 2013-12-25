package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import scala.collection.generic.CanBuildFrom
import org.shaqal.sql.adapter.Adapter

case class Count(column: Option[Column]) extends SelectExpression {
  def columnAlias = column map(_.aliasName) getOrElse "count"
  def render(implicit adapter: Adapter) = s"count (${column map(_.fullName) getOrElse "*"}) as " + columnAlias
}

case class Max(column: Column) extends SelectExpression {
  def columnAlias = column.aliasName
  def render(implicit adapter: Adapter) = s"max (${column.fullName}) as $columnAlias"
}

case class Min(column: Column) extends SelectExpression {
  def columnAlias = column.aliasName
  def render(implicit adapter: Adapter) = s"min (${column.fullName}) as $columnAlias"
}

case class Avg(column: Column) extends SelectExpression {
  def columnAlias = column.aliasName
  def render(implicit adapter: Adapter) = s"avg (${column.fullName}) as $columnAlias"
}

trait AggregateFunctions { this: Query =>

  def count()(implicit c: -:[D]): Long = {
    val sexpr = Count(None)
    (c queryColl (selectSql(sexpr), rs => rs getLong sexpr.columnAlias, implicitly[CanBuildFrom[Nothing, Long, Seq[Long]]].apply())).head
  }

  def count(f: R => Col)(implicit c: -:[D]): Long =  {
    val sexpr = Count(Some(f(r)))
    (c queryColl (selectSql(sexpr), rs => rs getLong sexpr.columnAlias, implicitly[CanBuildFrom[Nothing, Long, Seq[Long]]].apply())).head
  }

  def max[U](f: R => Col { type T = U } )(implicit c: -:[D]): Option[U] = {
    val col = f(r)
    val sexpr = Max(col)
    (c queryColl (selectSql(sexpr), implicit rs => if (col.checkNull) None else Some(col.get), implicitly[CanBuildFrom[Nothing, Option[U], Seq[Option[U]]]].apply())).head
  }

}

