package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet
import scala.collection.Factory

trait MapperQuery[A] extends Query { builder =>

  type R <: ReadOnlyMapperLike[A]

  def selectSql = SelectSQL(r.*, r.fromItem, whereExpr)

  def query(f: A => Any)(implicit c: -:[D]) =
    c query (selectSql, r.reader, f)

  def into[Coll[_]](factory: Factory[A, Coll[A]])(implicit c: -:[D]) =
    c queryColl (selectSql, r.reader, factory.newBuilder)

  // TODO: generalize these with the select holder versions?

  def list()(implicit c: -:[D]) = into(List)

//  def set()(implicit c: -:[D]) = into[Set]

  def option()(implicit c: -:[D]) = list.headOption

  def get()(implicit c: -:[D]) = list.head
}

trait ReadOnlyMapperLike[A] extends ReadOnlyAccessorLike with MapperQuery[A] { mapper =>

  type QueryType = MapperQuery[A]

  //  trait This {
  //	def schema = m.schema    
  //  }

  val reader: Reader[A]

  object R { def apply(r: ResultSet => A) = new Reader[A] { def apply(rs: ResultSet) = r(rs) } }

  def where(whereFunc: R => Expr) = new MapperQuery[A] {
    type QueryType = mapper.QueryType
    type D = mapper.D
    type R = mapper.R
    val r = mapper.r
    def fromItem = mapper.fromItem
    override def whereExpr = mapper.whereExpr && whereFunc(r)
    def where(w: R => Expr) = mapper.where(w)
  }
}

trait DualMapperLike[A, B] extends AccessorLike with ReadOnlyMapperLike[A] {

  type R <: AccessorLike with ReadOnlyMapperLike[A]
  val writer: W[B]

  trait MapperWriter[X] extends Writer[X]

  type W[X] = MapperWriter[X]

  object MapperWriter extends AccessorWriterFactory {
    implicit val mapperWriter = writer
    implicit object MapperValueWriter extends AccessorValueWriter with W[Value]
    implicit object MapperValuesWriter extends AccessorValuesWriter with MapperWriter[Values]
  }

  object RW {
    def apply(r: ResultSet => A, w: B => Seq[ColumnParam]) = {
      val reader = new Reader[A] { def apply(rs: ResultSet) = r(rs) }
      val writer = new MapperWriter[B] { def apply(b: B) = w(b) }
      (reader, writer)
    }
  }

}

trait ReadOnlyMapper[A] extends ReadOnlyMapperLike[A] with MapperQuery[A] {
  override type QueryType = MapperQuery[A]
  type R = this.type
  val r: R = this
}

trait DualMapper[A, B] extends DualMapperLike[A, B] with MapperQuery[A] { mapper =>

  override type QueryType = MapperQuery[A]
  type R = this.type
  val r: R = this

  override def where(whereFunc: R => Expr) = new MapperQuery[A] {
    type QueryType = mapper.QueryType
    type D = mapper.D
    type R = mapper.R
    val r = mapper.r
    def fromItem = mapper.fromItem
    override def whereExpr = mapper.whereExpr && whereFunc(r)
    def where(w: R => Expr) = mapper.where(w)
  }

}

trait Mapper[A] extends DualMapper[A, A]

