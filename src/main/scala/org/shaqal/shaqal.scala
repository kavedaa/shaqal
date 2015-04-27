package org.shaqal

import org.shaqal.sql._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._
import scala.util.Try

trait Database {

  type D <: Database

  def db: Database { type D = Database.this.D } = this

  class Schema(val name: String) extends SchemaLike {
    type D = Database.this.D
    def database = db
    def schemaName = Some(name)
  }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T)(implicit c: -:[D]): Try[T] = {
    c transaction tx
  }

  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T)(implicit c: -:[D]): Try[T] = {
    c autoTransaction tx
  }

  object InformationSchema extends Schema("INFORMATION_SCHEMA") with InformationSchema

  def tableExists(table: TableLike)(implicit c: -:[D]): Boolean = {
    table.schema.schemaName match {
      case Some(schemaName) =>
        InformationSchema.Tables where (t => (t.table_Schema is schemaName) && (t.table_Name is table.tableName)) exists ()
      case None =>
        InformationSchema.Tables where (t => t.table_Name is table.tableName) exists ()
    }
  }

}

trait SchemaLike {
  type D <: Database
  def database: Database { type D = SchemaLike.this.D }
  def schemaName: Option[String]

  class Table(name: String) extends TableLike {
    val schema: SchemaLike { type D = SchemaLike.this.D } = SchemaLike.this
    def tableName = name
  }
}

trait TableLike {
  val schema: SchemaLike
  def database: Database { type D = schema.D } = schema.database
  type D = schema.D

  def tableName: String
  def path = List(schema.schemaName, Some(tableName)).flatten

  //  TODO reconsider naming here

  def fullName(implicit adapter: Adapter) = path map adapter.identifier mkString "."

  def underscoreName = path mkString "_"

  def aliasPath: Seq[TableLike] = Seq(this)

  def aliasName = (aliasPath flatMap (_.path) mkString "_").toLowerCase + "$"

  def fullNameAndAlias(implicit adapter: Adapter) = Seq(fullName, "as", aliasName) mkString " "

  implicit val tableLike = this

  def tableExists()(implicit c: -:[D]) = database tableExists this

  override def toString = path mkString "."
}

trait DefaultSchema extends SchemaLike { this: Database =>
  //  type D = Database.this.D
  def database = db
  def schemaName = None
}

class CancelException extends RuntimeException
