package org.shaqal.sql.adapter

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.ColumnDefinitionElements.Identity
import java.sql.Types
import org.shaqal.sql.adapter.common.AdapterCommons

abstract class PostgreSQLAdapter extends Adapter {

  def identifier(s: String) = List("\"", s, "\"").mkString

  def identity = ""

  val serials = Map(
    Types.SMALLINT -> "smallserial",
    Types.INTEGER -> "serial",
    Types.BIGINT -> "bigserial")

  def columnDefinitionSql(definition: ColumnDefinition) =
    if (definition.elements contains Identity) {
      val serialDataType = serials get definition.sqlType getOrElse {
        throw new IllegalArgumentException("Identity not supported with " + dataType(definition.sqlType) + ", only with int, smallint or bigint.")
      }
      new AdapterCommons.ColumnDefinitionSQL(definition.columnName, serialDataType, definition.elements.toList filter (_ != Identity))
    }
    else new AdapterCommons.ColumnDefinitionSQL(definition.columnName, definition dataTypeName this, definition.elements.toList)

  def createSchemaSql(name: String) = AdapterCommons createSchemaSql name

  def dropSchemaSql(name: String) = AdapterCommons dropSchemaSql name
  
  def tableExists(table: TableLike)(implicit c: -:[Database]) = AdapterCommons tableExists table
  
  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]) = AdapterCommons schemaExists schema  

}

object PostgreSQLAdapter extends PostgreSQLAdapter {

  def createTableSql(table: TableLike, columnDefs: Seq[SingleSQL]) =
    throw new UnsupportedOperationException("Only supported on PostgreSQL 9.1 or higher, use PostgreSQL9Adapter if applicable.")

  def dropTableSql(table: TableLike) =
    throw new UnsupportedOperationException("Only supported on PostgreSQL 9.1 or higher, use PostgreSQL9Adapter if applicable.")
}

object PostgreSQL9Adapter extends PostgreSQLAdapter {

  def createTableSql(table: TableLike, columnDefs: Seq[SingleSQL]) =
    AdapterCommons createTableSql (table, columnDefs)

  def dropTableSql(table: TableLike) =
    AdapterCommons dropTableSql table
}
