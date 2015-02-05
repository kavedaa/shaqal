package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import java.sql._
import java.util.Date

abstract class Col(val columnName: String, val sqlType: Int)
  extends Column with ColumnDefinition with Field with ReadWritable { col =>

  lazy val Null = new Null(sqlType)

  def cols = Seq(this)

  def checkNull(implicit rs: ResultSet) = ((rs getObject aliasName) == null)
  //  def is(other: Col { type T = col.T } ) = new JoinExpr(this, other) 

  def valueParam(value: T): Param[_]
  
  def is(value: T) = Eq(this, valueParam(value))
  def isnt(value: T) = Ne(this, valueParam(value))  
  
//  def is(value: T): Expr
//  def isnt(value: T): Expr
  //  def in(values: List[T]) = In(this, values)

  def is(that: Col.ColOf[T]) = new JoinTerm(this, that)
  
  def auto(implicit rs: ResultSet): T
  def auto(colIndex: Int)(implicit rs: ResultSet): T

  override def toString = s"Col [ $columnName ]"
}

object Col {
  type ColOf[TT] = Col { type T = TT }
}

trait notnull extends NotNullReadWritable { this: Col =>
  
  addElement(ColumnDefinitionElements.NotNullable)
}

trait nullable extends NullableReadWritable { this: Col =>
  
  def isNull = IsNull(this)
  def isNotNull = IsNotNull(this)
  
  def is(value: Option[T]) = value match {
    case Some(v) => Eq(this, valueParam(v))
    case None => IsNull(this)
  }
  
  def isnt(value: Option[T]) = value match {
    case Some(v) => Ne(this, valueParam(v)) || IsNull(this)
    case None => IsNotNull(this)
  }
  
  addElement(ColumnDefinitionElements.Nullable)
}
