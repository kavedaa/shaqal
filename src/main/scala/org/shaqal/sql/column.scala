package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.Util._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

trait ColumnLike {
  val columnName: String
  def sqlType: Int
  def hasGeneratedValue = false
}

trait Column extends ColumnLike {

  def table: TableLike

  def path = table.path :+ columnName
  def fullName(implicit adapter: Adapter) = path map adapter.identifier mkString "."
  def aliasName = path mkString "_"
  def render(implicit adapter: Adapter) = List(fullName, "as", aliasName) mkString " "
}

class ColumnTerm(column: ColumnLike, fromItem: FromItem) {
  
}
