package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.pretty._
import org.shaqal.sql.adapter._

case class DeleteSQL(table: TableLike, where: Expr) extends SingleSQL {

  implicit val cf: ColumnFormat = ColumnFormat.Name
  
  def render(implicit adapter: Adapter) = List(
    "delete from",
    table.fullName) :::
    (if (where == True)
      Nil
    else
      List("where", Expr render where)) mkString " "

  def params = where.params

  override def pp(implicit adapter: Adapter) =
    ElementList(
      "delete from",
      Indent(table.fullName)) :::
      (if (where != True)
        ElementList(
        "where",
        Indent(Expr pp where))
      else
        ElementList())
}