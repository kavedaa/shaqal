package org.shaqal.sql.adapter.common

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.Util._
import org.shaqal.sql.pretty._
import org.shaqal.sql.adapter.Adapter

abstract class CreateTableSQL(tableName: TableName, columnDefs: Seq[SingleSQL])
  extends SingleSQL {

  val instruction: String

  def createRender(implicit adapter: Adapter) =
    List(
      instruction,
      tableName.render,
      columnDefs map (_.render) mkString ", " parens) mkString " "

  def createPp(implicit adapter: Adapter) = ElementList(
    List(instruction, tableName.render) mkString " ",
    Indent(
      CommaLines(columnDefs.toList map (_.render)).parens))

}

object AdapterCommons {

  class ColumnDefinitionSQL(columnName: String, dataType: String, elements: List[ColumnDefinitionElement[_]]) extends SingleSQL {

    def render(implicit adapter: Adapter) =
      ((adapter identifier columnName) :: dataType :: (elements map (_.render))) mkString " "

    def params = elements flatMap (_.params)
  }

  class AddConstraintSQL(tableName: TableName, constraint: SingleSQL)
    extends SingleSQL {

    def render(implicit adapter: Adapter) =
      Seq(
        "alter table",
        tableName.render,
        "add",
        constraint.render) mkString " "

    def params = constraint.params
  }

  def createTableSql(tableName: TableName, columnDefs: Seq[SingleSQL]) = new CreateTableSQL(tableName, columnDefs) {

    val instruction = "create table if not exists"

    def render(implicit adapter: Adapter) = createRender

    override def pp(implicit adapter: Adapter) = createPp

    def params = columnDefs flatMap (_.params)
  }

  def dropTableSql(tableName: TableName) = new SingleSQL {
    def render(implicit adapter: Adapter) = s"drop table if exists ${tableName.render}"
    def params = Nil
  }

  def createSchemaSql(name: String) = SQL { s"create schema if not exists $name" }
  
  def dropSchemaSql(name: String) = SQL { s"drop schema if exists $name" }
  
  def tableExists(table: TableLike)(implicit c: -:[Database]) = {
    val sql = table.schema.schemaName match {
      case Some(schemaName) =>
        new SingleSQL {
          def render(implicit adapter: Adapter) = "select 1 from information_schema.tables where table_name = (?) and table_schema = (?)"
          def params = Seq[StringParam](table.tableName.toUpperCase, schemaName.toUpperCase)
        }
      case None =>
        new SingleSQL {
          def render(implicit adapter: Adapter) = "select 1 from information_schema.tables where table_name = (?)"
          def params = Seq[StringParam](table.tableName.toUpperCase)
        }
    }
    c.queryElement(sql, identity).isDefined
  }

  def schemaExists(schema: Database#Schema)(implicit c: -:[Database]) = {
    val sql = new SingleSQL {
      def render(implicit adapter: Adapter) = "select 1 from information_schema.schemata where schema_name = (?)"
      def params = Seq(StringParam(schema.name.toUpperCase))
    }
    c.queryElement(sql, identity).isDefined
  }

}