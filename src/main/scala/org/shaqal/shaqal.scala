package org.shaqal

import org.shaqal.sql._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._
import scala.util.Try

trait Database { db =>

  type D <: Database

  class Schema(val name: String) extends SchemaLike {
    type D = db.D
    //    def database = db
    def schemaName = Some(name)
  }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T)(implicit c: -:[D]): Try[T] = {
    c transaction tx
  }

  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T)(implicit c: -:[D]): Try[T] = {
    c autoTransaction tx
  }

}

trait SchemaLike { sch =>
  type D <: Database
  //  def database: Database { type D = sch.D }
  def schemaName: Option[String]

  class Table(name: String) extends TableLike {
    val schema: SchemaLike { type D = sch.D } = sch
    def tableName = name
  }
}

trait TableLike { tbl =>
  val schema: SchemaLike
  //  def database: Database { type D = schema.D } = schema.database
  type D = schema.D

  def tableName: String

  protected def tableAlias = tableName

  protected def schemaTableName = List(schema.schemaName, Some(tableName)).flatten
  private def schemaTableAlias = List(schema.schemaName, Some(tableAlias)).flatten

  //  TODO reconsider naming here

  def fullName(implicit adapter: Adapter) = schemaTableName map adapter.identifier mkString "."

  def underscoreName = schemaTableName mkString "_"

  def aliasPath: Seq[TableLike] = Seq(this)

  def aliasName = (aliasPath flatMap (_.schemaTableAlias) mkString "_").toLowerCase + "$"

  def fullNameAndAlias(implicit adapter: Adapter) = Seq(fullName, "as", aliasName) mkString " "

  implicit val tableLike = this

  override def toString = schemaTableName mkString "."
}

trait DefaultSchema extends SchemaLike { db: Database =>
  //  def database = db
  def schemaName = None
}

class CancelException extends RuntimeException
