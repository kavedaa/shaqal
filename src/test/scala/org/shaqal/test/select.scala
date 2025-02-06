package org.shaqal.test

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class SelectTest extends AnyFunSuite with BeforeAndAfter with Matchers {

  implicit def dbc: DBC[TestDB]

  trait TestTableAccessor extends Accessor with TableDefinition {

    val id = new int("id") with identity
    val name = new varchar(100)("name") with notnull
    val age = new int("age") with nullable

    def fields = Seq(id, name, age)

    //	experimental
    //  val twiceAge = age map(_ * 2)
    //  val ageAsDouble = age.as[Double]  

    def constraints = Nil
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB

    object TestTable extends Table("TestTable") with TestTableAccessor
  }

  import DB._

  before {

    TestTable.drop(true)
    TestTable.createTable()

    TestTable insert TestTable.Values(t => Seq(t.name := "John", t.age := Some(34)))
  }

  test("single") {
    TestTable.select(_.name).option() shouldEqual Some("John")
    TestTable.select(_.age).option() shouldEqual Some(Some(34))
    val query = TestTable.select(_.name)
    // val name = query[Vector]()
    // val name1 = TestTable.select(_.name).apply[Vector]()
  }

  test("tuple2") {
    TestTable.select(t => (t.name, t.age)).option() shouldEqual Some(("John", Some(34)))
  }

//  test("experimental") {
//    TestTable select (_.twiceAge) option () should equal(Some(Some(68)))
//    TestTable select (_.ageAsDouble) option () should equal(Some(Some(34.0)))
//  }

  //  test("tuple3") {
  //    DT select(dt => (dt.intTest, dt.intTest, dt.intTest)) option() should equal(Some((123456, 123456)))
  //    DT select(dt => (dt.intTest, dt.intTestNullable)) option() should equal(Some((123456, Some(123456))))
  //  }

}