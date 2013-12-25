package org.shaqal.test

import org.shaqal._
import org.scalatest.FunSuite
import org.shaqal.test.db.TestDB
import org.scalatest.matchers.ShouldMatchers

abstract class DefinitionTest extends FunSuite with ShouldMatchers {

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

  test("table definition") {

    TestTable create (name => println(s"Created table: $name."))
    TestTable tableExists () should be(true)

    TestTable drop true
    TestTable tableExists () should be(false)
  }

  def andPrint(name: String) { println (name) }
  
  test("schema definition") {

    TestSchema create andPrint
    TestSchema schemaExists () should be(true)

    TestSchema drop true
    TestSchema schemaExists () should be(false)
  }

  test("table definition in schema") {

    TestSchema create andPrint
    
    TestSchema.TestTable create andPrint
    TestSchema.TestTable tableExists () should be(true)

    TestSchema.TestTable drop true
    TestSchema.TestTable tableExists () should be(false)
    
    TestSchema drop true    
  }

}