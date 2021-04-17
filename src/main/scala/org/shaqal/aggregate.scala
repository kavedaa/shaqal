package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import scala.collection.generic.CanBuildFrom
import org.shaqal.sql.adapter.Adapter

abstract class AggregateExpression extends SelectExpression {
  implicit val cf: ColumnFormat = ColumnFormat.TableAlias    
}

case class Count(column: Option[Column]) extends AggregateExpression {
  def columnAlias = column map(_.aliasName) getOrElse "count"
  def render(implicit adapter: Adapter) = s"count (${column map(_.render) getOrElse "*"}) as " + columnAlias
}

case class Max(column: Column) extends AggregateExpression {
  def columnAlias = column.aliasName
  def render(implicit adapter: Adapter) = s"max (${column.render}) as $columnAlias"
}

case class Min(column: Column) extends AggregateExpression {
  def columnAlias = column.aliasName
  def render(implicit adapter: Adapter) = s"min (${column.render}) as $columnAlias"
}

case class Avg(column: Column) extends AggregateExpression {
  def columnAlias = column.aliasName
  def render(implicit adapter: Adapter) = s"avg (${column.render}) as $columnAlias"
}

trait AggregateFunctions { this: Query =>

  def count()(implicit c: -:[D]): Long = {
    val sexpr = Count(None)
    (c queryColl (selectSql(sexpr), rs => rs getLong sexpr.columnAlias, List.newBuilder[Long])).head
  }

  def count(f: R => Col)(implicit c: -:[D]): Long =  {
    val sexpr = Count(Some(f(r)))
    (c queryColl (selectSql(sexpr), rs => rs getLong sexpr.columnAlias, List.newBuilder[Long])).head
  }

  def max[U](f: R => Col { type T = U })(implicit c: -:[D]): Option[U] = {
    val col = f(r)
    val sexpr = Max(col)
    (c queryColl (selectSql(sexpr), implicit rs => if (col.checkNull) None else Some(col.get), List.newBuilder[Option[U]])).head
  }

  //  workaround for https://github.com/scala/bug/issues/10407
  def maxCompat[U](col: Col { type T = U })(implicit c: -:[D]): Option[U] = {
//    val col = f(r)
    val sexpr = Max(col)
    (c queryColl (selectSql(sexpr), implicit rs => if (col.checkNull) None else Some(col.get), List.newBuilder[Option[U]])).head
  }

}

