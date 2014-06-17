package org.shaqal

import org.shaqal.sql._
import org.shaqal.sql.Util._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

trait InsertSQL { this: SQL =>

  val tableName: TableName
  val columns: Seq[ColumnLike]

  val keywordInsertInto = "insert into"
  val keywordValues = "values"

  def render(implicit adapter: Adapter) = List(
    keywordInsertInto,
    tableName.fullName,
    columns map (adapter identifier _.columnName) mkString ", " parens,
    keywordValues,
    columns map (_ => "?") mkString ", " parens) mkString " "

  override def pp(implicit adapter: Adapter) = ElementList(
    keywordInsertInto,
    Indent(
      tableName.fullName,
      Indent(CommaLines(columns map (adapter identifier _.columnName) toList).parens)),
    keywordValues + " " + (columns map (_ => "?") mkString ", " parens))
}

class SingleInsertSQL(val tableName: TableName, val columns: Seq[Column], columnParams: Seq[ColumnParam])
  extends SingleSQL with InsertSQL {
  def params = columnParams map (_.param)
}

class BatchInsertSQL(val tableName: TableName, val columns: Seq[Column], paramsSeqs: Seq[Seq[Param[_]]])
  extends BatchSQL with InsertSQL {
  def params = paramsSeqs map ParamsSeq.apply
}

object InsertSQL {

  def apply(tableName: TableName, columnParams: Seq[ColumnParam]): SingleInsertSQL =
    new SingleInsertSQL(tableName, columnParams map (_.column), columnParams)

  def apply(tableName: TableName, columns: Seq[Column], paramsSeqs: Seq[Seq[Param[_]]]): BatchInsertSQL =
      new BatchInsertSQL(tableName, columns, paramsSeqs)

}
