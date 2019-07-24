package org.shaqal.sql

import java.sql._

abstract class Param[T] {
  def set(ps: PreparedStatement, index: Int)
  def render: String
}

class Null(sqlType: Int) extends Param[Nothing] {
  def set(ps: PreparedStatement, index: Int) { ps setNull (index, sqlType) }
  def render = "NULL"
}

abstract class ValueParam[T] extends Param[T] {
  val v: T
  def render = v.toString
}

case class StringParam(v: String) extends ValueParam[String] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setString (index, v)
  }
}

case class ShortParam(v: Short) extends ValueParam[Short] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setShort (index, v)
  }
}

case class IntParam(v: Int) extends ValueParam[Int] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setInt (index, v)
  }
}

case class LongParam(v: Long) extends ValueParam[Long] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setLong (index, v)
  }
}

case class DoubleParam(v: Double) extends ValueParam[Double] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setDouble (index, v)
  }
}

case class TimestampParam(v: java.util.Date) extends ValueParam[java.util.Date] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setTimestamp (index, new Timestamp(v.getTime))
  }
}

case class DateParam(v: java.sql.Date) extends ValueParam[java.sql.Date] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setDate(index, v)
  }
}

case class BigDecimalParam(v: BigDecimal) extends ValueParam[BigDecimal] {
  //	TODO: is there a better way to convert scala BigDecimal to java BigDecimal?
  def set(ps: PreparedStatement, index: Int) = {
    ps setBigDecimal (index, new java.math.BigDecimal(v.toString))
  }
  //   Yes like this:

  val jb = v.bigDecimal

  // And other way:

  val sb2 = BigDecimal(jb)
}

case class BooleanParam(v: Boolean) extends ValueParam[Boolean] {
  def set(ps: PreparedStatement, index: Int) = {
    ps setBoolean (index, v)
  }
}

object Param {
  implicit def apply(v: Short) = new ShortParam(v)
  implicit def apply(v: Int) = new IntParam(v)
  implicit def apply(v: Long) = new LongParam(v)
  implicit def apply(v: Double) = new DoubleParam(v)
  implicit def apply(v: String) = new StringParam(v)
  implicit def apply(v: java.util.Date) = new TimestampParam(v)
  implicit def apply(v: java.sql.Date) = new DateParam(v)
  implicit def apply(v: BigDecimal) = new BigDecimalParam(v)
  implicit def apply(v: Boolean) = new BooleanParam(v)
}

case class ColumnParam(column: Column, param: Param[_])
