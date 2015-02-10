package org.shaqal.sql.adapter

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.adapter.mssql._
import java.sql.Types
import org.shaqal.sql.adapter.common.AdapterCommons

object MSSQLAdapter extends Adapter {

  dataTypes += Types.DOUBLE -> "float"
  
  def identifier(s: String) = List("[", s, "]") mkString

  def identity = "identity primary key"

  def columnDefinitionSql(definition: ColumnDefinition) =
    new AdapterCommons.ColumnDefinitionSQL(definition.columnName, definition dataTypeName this, definition.elements.toList)

  def createTableSql(table: TableLike, columnDefs: Seq[SingleSQL]) =
    new CreateTableSQL(table, columnDefs)

  def dropTableSql(table: TableLike) =
    new DropTableSQL(table)

  def createSchemaSql(name: String) = 
    new CreateSchemaSQL(name)
  
  def dropSchemaSql(name: String) = 
    new DropSchemaSQL(name)
  
  def tableExists(table: TableLike)(implicit c: -:[Database]) = AdapterCommons tableExists table
  
  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]) = AdapterCommons schemaExists schema
  
}