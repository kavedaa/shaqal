package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class PK2MapperCrudTest extends FeatureSpec with BeforeAndAfter with Matchers {

  case class City(id1: Int, id2: Int, name: String)

  trait CityMapper extends PK2Mapper[City, Int, Int] with TableDefinition {
    val id1 = new int("id1") with notnull
    val id2 = new int("id2") with notnull
    val name = new varchar(100)("name") with notnull
    def fields = Seq(id1, id2, name)
    val (pk, pkf) = PK((id1, id2), c => (c.id1, c.id2))
    val (reader, writer) = RW(implicit rs => City(id1.read, id2.read, name.read), c => Seq(id1 := c.id1, id2 := c.id2, name := c.name))
    def constraints = Nil
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB
    object City extends Table("City") with CityMapper
  }

  implicit def dbc: DBC[TestDB]

  before {
    DB.City createTable ()
  }

  after {
    DB.City drop true
  }

  val london = City(1, 1, "London")
  val paris = City(1, 2, "Paris")
  val berlin = City(2, 1, "Berlin")

  feature("update") {

    scenario("the item exists") {

      DB.City insertAll Seq(london, paris)

      val num = DB.City update City(1, 1, "Londinium")
      num should equal(1)
      DB.City list () should equal(List(City(1, 1, "Londinium"), paris))
    }

    scenario("the item does not exist") {

      DB.City insertAll Seq(london, paris)

      val num = DB.City update berlin
      num should equal(0)
      DB.City list () should equal(List(london, paris))
    }

  }

}