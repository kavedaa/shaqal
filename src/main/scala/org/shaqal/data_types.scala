package org.shaqal

import java.sql._
import org.shaqal.sql._

abstract class SmallIntCol(name: String) extends Col(name, Types.SMALLINT) {

  type T = Short
  
  def fullDataType(typeName: String) = typeName
  
	//	write	  	
  
  def set(v: Short) = ColumnParam(this, ShortParam(v))
  def set(v: Option[Short]) = ColumnParam(this, v map ShortParam.apply getOrElse Null)

  //	read
  
  def get(implicit rs: ResultSet) = rs getShort aliasName

  def auto(implicit rs: ResultSet) = rs getShort columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getShort colIndex

  def valueParam(value: Short) = new ShortParam(value)
  
  //	predicates
  
  def >(value: Short) = Gt(this, value)
  def >=(value: Short) = Gte(this, value)
  def <(value: Short) = Lt(this, value)
  def <=(value: Short) = Lte(this, value)
  
}

abstract class IntCol(name: String) extends Col(name, Types.INTEGER) {

  type T = Int
  
  def fullDataType(typeName: String) = typeName

  def set(v: Int) = ColumnParam(this, IntParam(v))
  def set(v: Option[Int]) = ColumnParam(this, v map IntParam.apply getOrElse Null)

  def valueParam(value: Int) = new IntParam(value)

//  def is(value: Int) = Eq(this, value)
//  def isnt(value: Int) = Ne(this, value)

//  def is(value: Option[Int]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
//  
//  def isnt(value: Option[Int]) = value match {
//    case Some(v) => Ne(this, v) || IsNull(this)
//    case None => IsNotNull(this)
//  }
  
  def >(value: Int) = Gt(this, value)
  def >=(value: Int) = Gte(this, value)
  def <(value: Int) = Lt(this, value)
  def <=(value: Int) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getInt aliasName

  def auto(implicit rs: ResultSet) = rs getInt columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getInt colIndex
}

abstract class BigIntCol(name: String) extends Col(name, Types.BIGINT) {

  type T = Long
  
  def fullDataType(typeName: String) = typeName

  def set(v: Long) = ColumnParam(this, LongParam(v))
  def set(v: Option[Long]) = ColumnParam(this, v map LongParam.apply getOrElse Null)

  def valueParam(value: Long) = new LongParam(value)

//  def is(value: Long) = Eq(this, value)
//  def isnt(value: Long) = Ne(this, value)

//  def is(value: Option[Long]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
//  
//  def isnt(value: Option[Long]) = value match {
//    case Some(v) => Ne(this, v) || IsNull(this)
//    case None => IsNotNull(this)
//  }
  
  def >(value: Long) = Gt(this, value)
  def >=(value: Long) = Gte(this, value)
  def <(value: Long) = Lt(this, value)
  def <=(value: Long) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getLong aliasName

  def auto(implicit rs: ResultSet) = rs getLong columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getLong colIndex
}

abstract class SingleCharCol(name: String) extends Col(name, Types.CHAR) {

  type T = Char
  
  def fullDataType(typeName: String) = typeName + "(1)"

  def set(v: Char) = ColumnParam(this, v.toString)
  def set(v: Option[Char]) = ColumnParam(this, v map (c => StringParam(c.toString)) getOrElse Null)

  def valueParam(value: Char) = new StringParam(value.toString)

//  def is(value: Char) = Eq(this, value.toString)
//  def isnt(value: Char) = Ne(this, value.toString)

//  def is(value: Option[Char]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
//  
//  def isnt(value: Option[Char]) = value match {
//    case Some(v) => Ne(this, v) || IsNull(this)
//    case None => IsNotNull(this)
//  }
  
  def get(implicit rs: ResultSet) = rs getString aliasName charAt 0

  def auto(implicit rs: ResultSet) = rs getString columnName charAt 0
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getString colIndex charAt 0
}

abstract class StringLikeCol(name: String, sqlType: Int, dataTypeName: String) extends Col(name, sqlType) {

  type T = String
  
  val length: DataLength
  
  def fullDataType(typeName: String) = List(Some(typeName), length.render map("(" + _ + ")")).flatten.mkString
  
  def set(v: String) = ColumnParam(this, v)
  def set(v: Option[String]) = ColumnParam(this, v map Param.apply getOrElse Null)

  def valueParam(value: String) = new StringParam(value)

  def like(value: String) = Like(this, valueParam(value))

//  def is(value: String) = Eq(this, value)
//  def isnt(value: String) = Ne(this, value)

//  def is(value: Option[String]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
  
  def get(implicit rs: ResultSet) = rs getString aliasName

  def auto(implicit rs: ResultSet) = rs getString columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getString colIndex
}

abstract class CharCol(name: String) extends StringLikeCol(name, Types.CHAR, "char")

abstract class VarcharCol(name: String) extends StringLikeCol(name, Types.VARCHAR, "varchar")

abstract class DoubleCol(name: String) extends Col(name, Types.DOUBLE) {
  
  type T = Double
  
  def fullDataType(typeName: String) = typeName

  def set(v: Double) = ColumnParam(this, v)
  def set(v: Option[Double]) = ColumnParam(this, v map Param.apply getOrElse Null)

  def valueParam(value: Double) = new DoubleParam(value)

//  def is(value: Double) = Eq(this, value)
//  def isnt(value: Double) = Ne(this, value)

//  def is(value: Option[Double]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
  
  def >(value: Double) = Gt(this, value)
  def >=(value: Double) = Gte(this, value)
  def <(value: Double) = Lt(this, value)
  def <=(value: Double) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getDouble aliasName

  def auto(implicit rs: ResultSet) = rs getDouble columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getDouble colIndex  
}

abstract class TimestampCol(name: String) extends Col(name, Types.TIMESTAMP) {

  type T = java.util.Date
  
  def fullDataType(typeName: String) = typeName
  
  def set(v: java.util.Date) = ColumnParam(this, v)
  def set(v: Option[java.util.Date]) = ColumnParam(this, v map Param.apply getOrElse Null)

  def valueParam(value: java.util.Date) = new TimestampParam(value)

//  def is(value: java.util.Date) = Eq(this, value)
//  def isnt(value: java.util.Date) = Ne(this, value)

//  def is(value: Option[java.util.Date]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
  
  def >(value: java.util.Date) = Gt(this, value)
  def >=(value: java.util.Date) = Gte(this, value)
  def <(value: java.util.Date) = Lt(this, value)
  def <=(value: java.util.Date) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getTimestamp aliasName

  def auto(implicit rs: ResultSet) = rs getTimestamp columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getTimestamp colIndex
}

abstract class DateCol(name: String) extends Col(name, Types.DATE) {

  type T = java.sql.Date
  
  def fullDataType(typeName: String) = typeName
  
  def set(v: java.sql.Date) = ColumnParam(this, v)
  def set(v: Option[java.sql.Date]) = ColumnParam(this, v map Param.apply getOrElse Null)

  def valueParam(v: java.sql.Date) = new DateParam(v)

//  def is(value: java.sql.Date) = Eq(this, value)
//  def isnt(value: java.sql.Date) = Ne(this, value)

//  def is(value: Option[java.sql.Date]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
  
  def >(value: java.sql.Date) = Gt(this, value)
  def >=(value: java.sql.Date) = Gte(this, value)
  def <(value: java.sql.Date) = Lt(this, value)
  def <=(value: java.sql.Date) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getDate aliasName

  def auto(implicit rs: ResultSet) = rs getDate columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getDate colIndex
}


abstract class NumericCol(name: String) extends Col(name, Types.NUMERIC) {
  
  type T = BigDecimal
  
  val precision: DataLength
  val scale: DataLength
  
  def dim = List(precision, scale) flatMap(_.render) mkString ", " match {
    case s if s.isEmpty => None
    case s => Some("(" + s + ")")
  }
  
  def fullDataType(typeName: String) = List(Some(typeName), dim).flatten.mkString

  def set(v: BigDecimal) = ColumnParam(this, v)
  def set(v: Option[BigDecimal]) = ColumnParam(this, v map Param.apply getOrElse Null)

  def valueParam(v: BigDecimal) = new BigDecimalParam(v)

//  def is(value: BigDecimal) = Eq(this, value)
//  def isnt(value: BigDecimal) = Ne(this, value)

//  def is(value: Option[BigDecimal]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
  
  def >(value: BigDecimal) = Gt(this, value)
  def >=(value: BigDecimal) = Gte(this, value)
  def <(value: BigDecimal) = Lt(this, value)
  def <=(value: BigDecimal) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getBigDecimal aliasName

  def auto(implicit rs: ResultSet) = rs getBigDecimal columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getBigDecimal colIndex  
}

abstract class BitCol(name: String) extends Col(name, Types.BIT) {
  
  type T = Boolean
  
  def fullDataType(typeName: String) = typeName

  def set(v: Boolean) = ColumnParam(this, v)
  def set(v: Option[Boolean]) = ColumnParam(this, v map Param.apply getOrElse Null)

  def valueParam(v: Boolean) = new BooleanParam(v)

//  def is(value: Boolean) = Eq(this, value)
//  def isnt(value: Boolean) = Ne(this, value)

//  def is(value: Option[Boolean]) = value match {
//    case Some(v) => Eq(this, v)
//    case None => IsNull(this)
//  }
  
  def >(value: Boolean) = Gt(this, value)
  def >=(value: Boolean) = Gte(this, value)
  def <(value: Boolean) = Lt(this, value)
  def <=(value: Boolean) = Lte(this, value)

  def get(implicit rs: ResultSet) = rs getBoolean aliasName

  def auto(implicit rs: ResultSet) = rs getBoolean columnName
  def auto(colIndex: Int)(implicit rs: ResultSet) = rs getBoolean colIndex  
}