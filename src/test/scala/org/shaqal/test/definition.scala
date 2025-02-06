package org.shaqal.test

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class DefinitionTest extends AnyFunSuite with Matchers with BeforeAndAfter {

  implicit def dbc: DBC[TestDB]

  trait TestTable extends Accessor with TableDefinition {

    val id = new int("id") with notnull
    val name = new varchar("name") with nullable

    def fields = Seq(id, name)

    def constraints = Nil
  }

  trait TestSchema extends SchemaLike with SchemaDefinition { this: Database#Schema =>
    object TestTable extends Table("TestTable") with TestTable
  }

  trait DefinitionDB extends Database with DefaultSchema {
    object TestTable extends Table("TestTable") with TestTable
    object TestSchema extends Schema("TestSchema") with TestSchema
  }

  object DefinitionDB extends DefinitionDB {
    type D = TestDB
  }

  import DefinitionDB._

  after {
    TestTable.drop(true)
    TestSchema.TestTable.drop(true)
    TestSchema.drop(true)
  }

  test("table definition") {

    TestTable.create()
    TestTable.tableExists shouldBe true

    TestTable.drop(true)
    TestTable.tableExists shouldBe false
  }

  //  def andPrint(name: String) { println (name) }

  test("schema definition") {

    TestSchema.create()
    TestSchema.schemaExists shouldBe true

    TestSchema.drop(true)
    TestSchema.schemaExists shouldBe false
  }

  test("table definition with schema") {

    TestSchema.create()

    TestSchema.TestTable.create()
    TestSchema.TestTable.tableExists shouldBe true

    TestSchema.TestTable.drop(true)
    TestSchema.TestTable.tableExists shouldBe false
  }

  test("mix same table name with and without schema") {

    TestSchema.create()
    TestSchema.TestTable.create()

    TestSchema.TestTable.tableExists shouldBe true
    TestTable.tableExists shouldBe false
  }
}