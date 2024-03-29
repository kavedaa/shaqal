package org.shaqal.sql.adapter.mssql

import org.shaqal.sql._
import org.shaqal.sql.Util._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._
import org.shaqal.TableLike

trait Exists { this: SQL =>

  val predicate: String

  val keywordBegin = "begin"
  val keywordEnd = "end"

  def existsStatement: String
  def existsParams: Seq[String]

  def statement(implicit adapter: Adapter): String
  def statementPp(implicit adapter: Adapter): Element = statement

  def render(implicit adapter: Adapter) =
    List(
      existsStatement,
//      keywordBegin,
      statement
//      keywordEnd
      ) mkString " "

  override def pp(implicit adapter: Adapter) =
    ElementList(
      existsStatement,
      Indent(ElementList(
//        keywordBegin
        Indent(statementPp)
//        keywordEnd
        )))

  def params: ParamsSeq = (existsParams map (p => StringParam(p)))
}

trait TableExists extends Exists { this: SQL =>

  val table: TableLike

  def existsStatement = table.schema.schemaName match {
    case Some(schemaName) =>
      s"if ($predicate exists (select * from information_schema.tables where table_schema = (?) and table_name = (?)))"
    case None =>
      s"if ($predicate exists (select * from information_schema.tables where table_name = (?)))"
  }

  def existsParams = table.schema.schemaName match {
    case Some(schemaName) =>
      Seq(schemaName, table.tableName)
    case None => Seq(table.tableName)
  }
}

trait SchemaExists extends Exists { this: SQL =>

  val schemaName: String

  def existsStatement =
    s"if ($predicate exists (select * from information_schema.schemata where schema_name = (?)))"

  def existsParams = Seq(schemaName)
}

class CreateTableSQL(val table: TableLike, columnDefs: Seq[SingleSQL])
  extends org.shaqal.sql.adapter.common.CreateTableSQL(table, columnDefs)
  with TableExists {

  val predicate = "not"

  val instruction = "create table"

  def statement(implicit adapter: Adapter) = createRender

  override def statementPp(implicit adapter: Adapter) = createPp

  override def params = super[TableExists].params ++ (columnDefs.toList flatMap (_.params))
}

class DropTableSQL(val table: TableLike) extends SingleSQL with TableExists {

  val predicate = ""

  def statement(implicit adapter: Adapter) = s"drop table ${table.fullName}"
}

class CreateSchemaSQL(val schemaName: String) extends SingleSQL with SchemaExists {

  val predicate = "not"

  def statement(implicit adapter: Adapter) = s"exec('create schema $schemaName')"	// oh why MS
}

class DropSchemaSQL(val schemaName: String) extends SingleSQL with SchemaExists {

  val predicate = ""

  def statement(implicit adapter: Adapter) = s"drop schema $schemaName"
}