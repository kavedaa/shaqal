package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class CrudTest extends FeatureSpec with BeforeAndAfter with Matchers {

  trait PersonAccessor extends Accessor with TableDefinition {
    val name = new varchar(100)("name") with notnull
    val age = new int("age") with notnull
    def fields = Seq(name, age)
    def constraints = Nil
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB
    object Person extends Table("Person") with PersonAccessor
  }

  implicit def dbc: DBC[TestDB]

  before {
    DB.Person createTable ()
  }

  after {
    DB.Person drop true
  }

  val john = DB.Person.Values(c => Seq(c.name := "John", c.age := 30))
  val tom = DB.Person.Values(c => Seq(c.name := "Tom", c.age := 45))

  feature("exists") {

    scenario("without predicate & the element does not exist") {
      DB.Person exists () shouldBe false
    }

    scenario("without predicate & the element exists") {
      DB.Person insert john
      DB.Person.exists shouldBe true
    }

    scenario("with predicate & the element does not exist") {
      DB.Person insert john
      DB.Person where (_.name is "Tom") exists () shouldBe false
    }

    scenario("with predicate & the element exists") {
      DB.Person insert john
      DB.Person where (_.name is "John") exists () shouldBe true
    }
  }

  feature("insert - read") {

    scenario("read without predicate") {

      DB.Person insert john

      DB.Person select (c => (c.name, c.age)) option () shouldEqual Some("John", 30)

      DB.Person insert tom

      DB.Person select (c => (c.name, c.age)) set () shouldEqual Set(("John", 30), ("Tom", 45))
    }

    scenario("read with predicate") {

      DB.Person insert john
      DB.Person insert tom

      DB.Person where (_.name is "Tom") select (c => (c.name, c.age)) option () shouldEqual Some("Tom", 45)

    }
  }

  feature("insert - update - read") {

    scenario("update with predicate") {

      DB.Person insert john
      DB.Person insert tom

      DB.Person updateWhere (_.name is "John") set DB.Person.Value(_.age := 31)

      DB.Person select (c => (c.name, c.age)) set () shouldEqual Set(("John", 31), ("Tom", 45))
    }

  }

  feature("insert - delete - read") {

    scenario("delete with predicate") {

      DB.Person insert john
      DB.Person insert tom

      DB.Person deleteWhere (_.name is "John")

      val persons = DB.Person select (c => (c.name, c.age)) list ()
      persons.toSet shouldEqual Set(("Tom", 45))
    }
  }
}