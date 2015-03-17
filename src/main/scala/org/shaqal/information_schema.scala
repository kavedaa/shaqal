package org.shaqal

trait InformationSchema { this: SchemaLike =>

  object Tables extends Table("TABLES") with Accessor {

    val table_Schema = new varchar("TABLE_SCHEMA") with notnull
    val table_Name = new varchar("TABLE_NAME") with notnull

    def fields = Seq(table_Schema, table_Name)
  }

}