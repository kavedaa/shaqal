package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.pretty._
import org.shaqal.sql.adapter._

case class UpdateSQL(table: TableName, columnParams: Seq[ColumnParam], where: Expr) extends SingleSQL {
  
  def render(implicit adapter: Adapter) =
    List(
      "update",
      table.fullName,
      "set",
      (columnParams map(_.column.columnName + " = (?)") mkString ", "),
      "where",
      where.render) mkString " "
    
  def params = (columnParams map(_.param)) ++: where.params
  
  override def pp(implicit adapter: Adapter) = ElementList(
    "update",
    Indent(table.fullName),
    "set",
    Indent(
      CommaLines(columnParams.toList map(_.column.columnName + " = (?)"))),
    "where",
    Indent(where.pp))
}