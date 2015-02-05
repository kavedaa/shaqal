package org.shaqal

import org.shaqal.sql._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._
import scala.util.Try

trait Database {

  type D <: Database

  class Schema(val name: String) extends SchemaLike {
    type D = Database.this.D
    //    def database: D = Database.this
    def schemaName = Some(name)
  }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T)(implicit c: -:[D]): Try[T] = {
    c transaction tx
  }

  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T)(implicit c: -:[D]): Try[T] = {
    c autoTransaction tx
  }

}

trait SchemaLike {
  type D <: Database
  //  def database: D
  def schemaName: Option[String]

  class Table(name: String) extends TableLike {
    type D = SchemaLike.this.D
    def schema = SchemaLike.this
    def tableName = name
  }
}

trait TableLike {
  type D <: Database
  def schema: SchemaLike

  def tableName: String
  def path = List(schema.schemaName, Some(tableName)).flatten
  def fullName(implicit adapter: Adapter) = path map adapter.identifier mkString "."

  implicit val tableLike = this
}

trait DefaultSchema extends SchemaLike { this: Database =>
  //  type D = Database.this.D
  //  def database = this
  def schemaName = None
}

class CancelException extends RuntimeException
