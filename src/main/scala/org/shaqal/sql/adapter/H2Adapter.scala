package org.shaqal.sql.adapter

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.adapter.common.AdapterCommons

object H2Adapter extends Adapter {

  def identifier(s: String) = s

  def identity = "identity"

  def columnDefinitionSql(definition: ColumnDefinition) =
    new AdapterCommons.ColumnDefinitionSQL(definition.columnName, definition dataTypeName this, definition.elements.toList)
  
  def createTableSql(tableName: TableName, columnDefs: Seq[SingleSQL]) =
    AdapterCommons createTableSql(tableName, columnDefs)

  def dropTableSql(tableName: TableName) = AdapterCommons dropTableSql tableName

  def createSchemaSql(name: String) = AdapterCommons createSchemaSql name

  def dropSchemaSql(name: String) = AdapterCommons dropSchemaSql name

  //	for some reason the standard way of doing this doesn't work on H2
  def tableExists(table: TableLike)(implicit c: -:[Database]) = ???
  
  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]) = ???
  
}