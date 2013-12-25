package org.shaqal.test

import org.scalatest.FeatureSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfter
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class PKMapperCrudTest extends FeatureSpec with BeforeAndAfter with ShouldMatchers {

  case class Person(id: Int, name: String)

  trait PersonMapper extends PKMapper[Person, Int] with TableDefinition {
    val id = new int("id") with notnull
    val name = new varchar(100)("name") with notnull
    def fields = Seq(id, name)
    val (pk, pkf) = PK(id, _.id)
    val (reader, writer) = RW(implicit rs => Person(id.read, name.read), p => Seq(id := p.id, name := p.name))
    def constraints = Nil
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB
    object Person extends Table("Person") with PersonMapper
  }

  implicit def dbc: DBC[TestDB]

  before {
    DB.Person createTable ()
  }

  after {
    DB.Person drop true
  }

  val john = Person(1, "John")
  val tom = Person(2, "Tom")
  val lisa = Person(3, "Lisa")

  feature("read") {

    scenario("the item exists") {

      DB.Person insert john

      DB.Person(1) should equal(Some(john))
    }

    scenario("the item does not exist") {

      DB.Person insert john

      DB.Person(2) should equal(None)
    }
  }

  feature("update") {

    scenario("the item exists") {

      DB.Person insertAll Seq(john, tom)

      val num = DB.Person update Person(1, "Johnny")
      num should equal(1)
      DB.Person list () should equal(List(Person(1, "Johnny"), tom))
    }

    scenario("the item does not exist") {

      DB.Person insertAll Seq(john, tom)

      val num = DB.Person update lisa
      num should equal(0)
      DB.Person list () should equal(List(john, tom))
    }

  }

  feature("insertOrUpdate") {

    scenario("the item exists") {

      DB.Person insertAll Seq(john, tom)

      val autoOrNum = DB.Person insertOrUpdate Person(1, "Johnny")
      autoOrNum should equal(Right(1))
      DB.Person list () should equal(List(Person(1, "Johnny"), tom))
    }

    scenario("the item does not exist") {

      DB.Person insertAll Seq(john, tom)

      val autoOrNum = DB.Person insertOrUpdate lisa
      autoOrNum should equal(Left(None))
      DB.Person list () should equal(List(john, tom, lisa))
    }

  }

  feature("delete") {

    scenario("the item exists") {

      DB.Person insertAll Seq(john, tom)

      val num = DB.Person delete john
      num should equal(1)
      DB.Person list () should equal(List(tom))
    }

    scenario("the item does not exist") {

      DB.Person insertAll Seq(john, tom)

      val num = DB.Person delete lisa
      num should equal(0)
      DB.Person list () should equal(List(john, tom))
    }
  }

}