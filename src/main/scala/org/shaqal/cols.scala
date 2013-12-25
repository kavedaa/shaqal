package org.shaqal

import org.shaqal.sql.ColumnDefinition
import org.shaqal.sql.DataLength

abstract class smallint(name: String)(implicit t: TableLike)
  extends SmallIntCol(name) with ColumnDefinition {
  def table = t
}

abstract class int(name: String)(implicit t: TableLike)
  extends IntCol(name) with ColumnDefinition {
  def table = t
}

abstract class bigint(name: String)(implicit t: TableLike)
  extends BigIntCol(name) with ColumnDefinition {
  def table = t
}

abstract class char1(name: String)(implicit t: TableLike)
  extends SingleCharCol(name) with ColumnDefinition {
  def table = t
}

abstract class char(val length: DataLength)(name: String)(implicit t: TableLike)
  extends CharCol(name) with ColumnDefinition {
  def this(name: String)(implicit t: TableLike) = this(new DataLength(None))(name)
  def table = t
}

abstract class varchar(val length: DataLength)(name: String)(implicit t: TableLike)
  extends VarcharCol(name) with ColumnDefinition {
  def this(name: String)(implicit t: TableLike) = this(new DataLength(None))(name)
  def table = t
}

abstract class double(name: String)(implicit t: TableLike)
  extends DoubleCol(name) with ColumnDefinition {
  def table = t
}


abstract class timestamp(name: String)(implicit t: TableLike)
  extends TimestampCol(name) with ColumnDefinition {
  def table = t
}

abstract class numeric(val precision: DataLength, val scale: DataLength)(name: String)(implicit t: TableLike)
  extends NumericCol(name) with ColumnDefinition {
  def this(precision: DataLength)(name: String)(implicit t: TableLike) = this(precision, new DataLength(None))(name)
  def this(name: String)(implicit t: TableLike) = this(new DataLength(None), new DataLength(None))(name)
  def table = t

}