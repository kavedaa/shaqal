package org.shaqal.sql.adapter

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.adapter.common.AdapterCommons

object H2Adapter extends Adapter {

  def identifier(s: String) = List("\"", s, "\"").mkString

  def identity = "identity" 

  def columnDefinitionSql(definition: ColumnDefinition) =
    new AdapterCommons.ColumnDefinitionSQL(definition.columnName, definition dataTypeName this, definition.elements.toList)
  
  def createTableSql(table: TableLike, columnDefs: Seq[SingleSQL]) =
    AdapterCommons createTableSql(table, columnDefs)

  def dropTableSql(table: TableLike) = AdapterCommons dropTableSql table

  def createSchemaSql(name: String) = AdapterCommons createSchemaSql name

  def dropSchemaSql(name: String) = AdapterCommons dropSchemaSql name

  def tableExists(table: TableLike)(implicit c: -:[Database]) = AdapterCommons tableExists table
  
  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]) = AdapterCommons schemaExists schema
  
}