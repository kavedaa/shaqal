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

sealed abstract class ColumnFormat {
  def render(c: Column)(implicit adapter: Adapter): String
}

object ColumnFormat {
  
  object Name extends ColumnFormat {
    def render(c: Column)(implicit adapter: Adapter) = 
      adapter identifier c.columnName 
  }
  
  object TableAlias extends ColumnFormat {
    def render(c: Column)(implicit adapter: Adapter) = 
      Seq(c.table.aliasName, Name render c) mkString "."
  }

  object Full extends ColumnFormat {
    def render(c: Column)(implicit adapter: Adapter) = 
      Seq(TableAlias render c, "as", c.aliasName) mkString " "
  }  
}

trait Column extends ColumnLike {

  def table: TableLike

  def aliasName = Seq(table.aliasName, columnName) mkString "_"
  def render(implicit columnFormat: ColumnFormat, adapter: Adapter) = columnFormat render this 
}

class ColumnTerm(column: ColumnLike, fromItem: FromItem) {
  
}
