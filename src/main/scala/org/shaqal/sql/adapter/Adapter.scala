package org.shaqal.sql.adapter

import org.shaqal._
import org.shaqal.sql._
import java.sql.Types
import org.shaqal.sql.adapter.common.AdapterCommons

abstract class Adapter {

  implicit val adapter = this

  def dataType(sqlType: Int) = dataTypes(sqlType)

  protected val dataTypes = collection.mutable.Map(
    Types.SMALLINT -> "smallint",
    Types.INTEGER -> "int",
    Types.BIGINT -> "bigint",
    Types.CHAR -> "char",
    Types.VARCHAR -> "varchar",
    Types.DOUBLE -> "double",
    Types.TIMESTAMP -> "datetime",
    Types.DATE -> "date",
    Types.NUMERIC -> "numeric",
    Types.BIT -> "bit")

  def identifier(s: String): String

  def identity: String

  def columnDefinitionSql(columnDefinition: ColumnDefinition): SingleSQL

  def createTableSql(
    table: TableLike,
    columnDefs: Seq[SingleSQL]): SingleSQL

  //	not sure if we need adapter for this
  def addConstraintSql(
    table: TableLike,
    constraint: SingleSQL) = new AdapterCommons.AddConstraintSQL(table, constraint)

  def dropConstraintSql(
    table: TableLike,
    constraintName: String) = new AdapterCommons.DropConstraintSQL(table, constraintName)

  def dropTableSql(table: TableLike): SingleSQL

  def createSchemaSql(name: String): SingleSQL

  def dropSchemaSql(name: String): SingleSQL

  //  def tableExists(table: TableLike)(implicit c: -:[Database]): Boolean

//  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]): Boolean
  
  def defaultSchemaName: String
  
  def informationSchemaObjects: Map[Symbol, String] = AdapterCommons.informationSchemaObjects
    
  class InformationSchema[DD <: Database] extends SchemaLike {    
    
    type D = DD
    
    def schemaName = Some(informationSchemaObjects('schInformationSchema))
    
    object Schemata extends Table(informationSchemaObjects('schSchemata)) with Accessor {
      
      val schema_name = new varchar(informationSchemaObjects('colSchemaName)) with notnull
      
      def fields = Seq(schema_name)
    }
    
    object Tables extends Table(informationSchemaObjects('tblTables)) with Accessor {

      val table_schema = new varchar(informationSchemaObjects('colTableSchema)) with notnull
      val table_name = new varchar(informationSchemaObjects('colTableName)) with notnull

      def fields = Seq(table_schema, table_name)
    }
  }
    
}