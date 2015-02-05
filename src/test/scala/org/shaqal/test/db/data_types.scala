package org.shaqal.test.db

import org.shaqal._
import java.text.SimpleDateFormat

//	We prefer tests to be self-contained, 
//	but these classes could really benefit from being reused...

trait DataTypesNullable { this: TableLike =>

  val smallintTestNullable = new smallint("smallintTestNullable") with nullable
  val intTestNullable = new int("intTestNullable") with nullable
  val bigintTestNullable = new bigint("bigintTestNullable") with nullable
  val char1TestNullable = new char1("char1TestNullable") with nullable
  val charTestNullable = new char(3)("charTestNullable") with nullable
  val varcharTestNullable = new varchar(100)("varcharTestNullable") with nullable
  val doubleTestNullable = new double("doubleTestNullable") with nullable
  val timestampTestNullable = new timestamp("timestampTestNullable") with nullable
  val dateTestNullable = new date("dateTestNullable") with nullable
  val numericTestNullable = new numeric(5, 2)("numericTestNullable") with nullable
  val bitTestNullable = new bit("bitTestNullable") with nullable

  def nullableFields = Seq(
    smallintTestNullable,
    intTestNullable,
    bigintTestNullable,
    char1TestNullable,
    charTestNullable,
    varcharTestNullable,
    doubleTestNullable,
    timestampTestNullable,
    dateTestNullable,
    numericTestNullable,
    bitTestNullable)
}

trait DataTypesNotNullable { this: TableLike =>

  val smallintTest = new smallint("smallintTest") with notnull
  val intTest = new int("intTest") with notnull
  val bigintTest = new bigint("bigintTest") with notnull
  val char1Test = new char1("char1Test") with notnull
  val charTest = new char(3)("charTest") with notnull
  val varcharTest = new varchar(100)("varcharTest") with notnull
  val doubleTest = new double("doubleTest") with notnull
  val timestampTest = new timestamp("timestampTest") with notnull
  val dateTest = new date("dateTest") with notnull
  val numericTest = new numeric(5, 2)("numericTest") with notnull
  val bitTest = new bit("bitTest") with notnull

  def notNullableFields = Seq(
    smallintTest,
    intTest,
    bigintTest,
    char1Test,
    charTest,
    varcharTest,
    doubleTest,
    timestampTest,
    dateTest,
    numericTest,
    bitTest)
}

trait DataTypesNullableTable
  extends DataTypesNullable with Accessor with TableDefinition {

  val id = new int("id") with notnull
  
  def fields = id +: nullableFields

  def constraints = Nil
}

trait DataTypesTable
  extends DataTypesNullable with DataTypesNotNullable with Accessor with TableDefinition {

  val id = new int("id") with notnull
  
  def fields = id +: (nullableFields ++ notNullableFields)

  def constraints = Nil
}
