package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import java.sql._
import java.util.Date

//case class Converter[A, B](f: A => B)
//
//object Converter {
//  implicit val intToDouble = new Converter((i: Int) => i.toDouble)
//}

abstract class Col(val columnName: String, val sqlType: Int)
  extends Column with ColumnDefinition with Field with ReadWritable { col =>

  lazy val Null = new Null(sqlType)

  def cols = Seq(this)

  def checkNull(implicit rs: ResultSet) = ((rs getObject aliasName) == null)
  //  def is(other: Col { type T = col.T } ) = new JoinExpr(this, other) 

  def is(value: T): Expr
  def isnt(value: T): Expr
  //  def in(values: List[T]) = In(this, values)

  def auto(implicit rs: ResultSet): T
  def auto(colIndex: Int)(implicit rs: ResultSet): T

  //  def getAs[T](implicit rs: ResultSet, converter: Converter[C, T]) = converter f get
  //  def optAs[T](implicit rs: ResultSet, converter: Converter[C, T]) = opt map converter.f
  //  def autoAs[T](implicit rs: ResultSet, converter: Converter[C, T]) = converter f auto

  override def toString = s"Col [ $columnName ]"
}

object Col {
  type ColOf[TT] = Col { type T = TT }
  implicit def fromTuple[T](t: Tuple2[ColOf[T], ColOf[T]]) = new JoinExpr(t._1, t._2)

}

trait notnull extends NotNullReadWritable { this: Col =>
  addElement(ColumnDefinitionElements.NotNullable)
}

trait nullable extends NullableReadWritable { this: Col =>
  def isNull = IsNull(this)
  def isNotNull = IsNotNull(this)  
  addElement(ColumnDefinitionElements.Nullable)
}
