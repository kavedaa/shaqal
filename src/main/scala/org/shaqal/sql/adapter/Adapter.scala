package org.shaqal.sql.adapter

import org.shaqal._
import org.shaqal.sql._
import java.sql.Types
import org.shaqal.sql.adapter.common.AdapterCommons

abstract class Adapter {

  def dataType(sqlType: Int) = dataTypes(sqlType)

  protected val dataTypes = collection.mutable.Map(
    Types.SMALLINT -> "smallint",
    Types.INTEGER -> "int",
    Types.BIGINT -> "bigint",
    Types.CHAR -> "char",
    Types.VARCHAR -> "varchar",
    Types.DOUBLE -> "double",
    Types.TIMESTAMP -> "datetime",
    Types.NUMERIC -> "numeric")

  def identifier(s: String): String

  def identity: String

  def columnDefinitionSql(columnDefinition: ColumnDefinition): SingleSQL
  
  def createTableSql(
    tableName: TableName,
    columnDefs: Seq[SingleSQL]): SingleSQL

    //	not sure if we need adapter for this
  def addConstraintSql(
    tableName: TableName,
    constraint: SingleSQL) = new AdapterCommons.AddConstraintSQL(tableName, constraint)

  def dropTableSql(tableName: TableName): SingleSQL

  def createSchemaSql(name: String): SingleSQL

  def dropSchemaSql(name: String): SingleSQL

  def tableExists(table: TableLike)(implicit c: -:[Database]): Boolean
  
  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]): Boolean
}