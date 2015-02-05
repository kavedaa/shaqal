package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet

abstract class Reader[X] extends (ResultSet => X)

abstract class Writer[X] extends (X => Seq[ColumnParam])

trait Query extends Selecting with AggregateFunctions with Adhoc { 

  type D <: Database
  type R
  val r: R
  
  type QueryType <: Query
  
  def fromItem: FromItem
  def whereExpr: Expr = True

  protected def selectSql(s: SelectExpression) = SelectSQL(s, fromItem, whereExpr)

  def where(w: R => Expr): QueryType {
    type QueryType = Query.this.QueryType
    type D = Query.this.D
    type R = Query.this.R
  }

  def exists()(implicit c: -:[D]): Boolean = count > 0
  
  def existsWhere(w: R => Expr)(implicit c: -:[D]): Boolean = where(w) exists()
}


trait ReadOnlyAccessorLike extends TableLike with Fields with Joining with Adhoc { r =>

  type R
  
  def * : Seq[Col] = cols ++: (foreigns flatMap(_.*))

  def fromItem: FromItem =
    if (foreigns.isEmpty)
      TableName(this)
    else
      new JoinedItem(TableName(this), foreigns map { f => new JoinElement(f.fromItem, f.joinType, f.joinExpr) })

  //	Using typeclass pattern to avoid method overloading (this method is used for FK constraints)
  def apply[Z](f: r.type => Z)(implicit columnator: TableColumnator[r.type, Z]) =
    columnator columnate (this, f)
    
}

trait AccessorLike extends ReadOnlyAccessorLike with Inserting with Updating with Deleting { r =>

  type GG

  def insertMethod(sql: SingleInsertSQL)(implicit c: -:[D]): Option[GG] = c insert (sql, Nil, None)
  def batchInsertMethod(sql: BatchInsertSQL)(implicit c: -:[D]): List[GG] = c batchInsert (sql, Nil, None)

  class AccessorWriterFactory {

    class AccessorValueWriter extends Writer[Value] {
      def apply(value: Value) = Seq(value.f(r))
    }

    class AccessorValuesWriter extends Writer[Values] {
      def apply(values: Values) = values.f(r)
    }

  }
}

trait ReadOnlyAccessor extends ReadOnlyAccessorLike with Query { relation =>

  type QueryType = Query
  type R = ReadOnlyAccessor.this.type
  val r: R = ReadOnlyAccessor.this

  def where(w: R => Expr) = new Query {
    type QueryType = ReadOnlyAccessor.this.QueryType
    type D = ReadOnlyAccessor.this.D
    type R = ReadOnlyAccessor.this.R
    val r = ReadOnlyAccessor.this.r
    def fromItem = ReadOnlyAccessor.this.fromItem
    override def whereExpr = ReadOnlyAccessor.this.whereExpr && w(r)
    def where(w: R => Expr) = ReadOnlyAccessor.this.where(w)
  }
  
}

trait Accessor extends AccessorLike with Query { relation =>

  type QueryType = Query
  type R = Accessor.this.type
  val r: R = Accessor.this

  trait AccessorWriter[A] extends Writer[A]

  type W[A] = AccessorWriter[A]
  
  val Writer = AccessorWriter

  object AccessorWriter extends AccessorWriterFactory {

    implicit object AccessorValueWriter extends AccessorValueWriter with AccessorWriter[Value]
    implicit object AccessorValuesWriter extends AccessorValuesWriter with AccessorWriter[Values]

    def apply[A](f: (r.type, A) => Seq[ColumnParam]) = new AccessorWriter[A] {
      def apply(a: A) = f(r, a)
    }
  }

  def where(w: R => Expr) = new Query {
    type QueryType = Accessor.this.QueryType
    type D = Accessor.this.D
    type R = Accessor.this.R
    val r = Accessor.this.r
    def fromItem = Accessor.this.fromItem
    override def whereExpr = Accessor.this.whereExpr && w(r)
    def where(w: R => Expr) = Accessor.this.where(w)
  }

  def join[R2](other: QueryOf[D, R2]) = new AdhocJoining[D, R, R2](r, other)            
  
    
  
}

trait Auto[G] { this: AccessorLike =>

  type GG = G

  def autos = cols filter (_.hasGeneratedValue)

  val autosReader: Option[ResultSet => G]

  object Auto { def apply(f: ResultSet => G) = Some(f) }

  override def insertMethod(sql: SingleInsertSQL)(implicit c: -:[D]): Option[G] =
    c insert (sql, autos map (_.columnName), autosReader)

  override def batchInsertMethod(sql: BatchInsertSQL)(implicit c: -:[D]): List[G] =
    c batchInsert (sql, autos map (_.columnName), autosReader)
}

