package org.shaqal.test.db

import org.shaqal._

trait TestDB extends Database

trait CommonTestDB extends Database with DefaultSchema {

  object DataTypesNullableTable extends Table("DataTypesNullableTable") with DataTypesNullableTable
  object DataTypesTable extends Table("DataTypesTable") with DataTypesTable 

}

object CommonTestDB extends CommonTestDB {
  type D = TestDB
}