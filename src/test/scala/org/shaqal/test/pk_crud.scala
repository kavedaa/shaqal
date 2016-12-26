package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB
import org.shaqal.sql.True

abstract class PKCrudTest extends FeatureSpec with BeforeAndAfter with ShouldMatchers {

  trait PersonAccessor extends Accessor with PK[Int] with TableDefinition {
    val id = new int("id") with notnull
    val name = new varchar(100)("name") with notnull
    def fields = Seq(id, name)
    val pk = id
    def constraints = Nil
  }

  trait CityAccessor extends Accessor with PK2[Int, Int] with TableDefinition {
    val id1 = new int("id1") with notnull
    val id2 = new int("id2") with notnull
    val name = new varchar(100)("name") with notnull
    def fields = Seq(id1, id2, name)
    val pk = (id1, id2)
    def constraints = Nil
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB
    object Person extends Table("Person") with PersonAccessor
    object City extends Table("City") with CityAccessor
  }

  implicit def dbc: DBC[TestDB]

  import DB._

  before {
    Person createTable ()
    City createTable ()
  }

  after {
    Person drop true
    City drop true
  }

  val john = Person.Values(p => Seq(p.id := 1, p.name := "John"))
  val tom = Person.Values(p => Seq(p.id := 2, p.name := "Tom"))

  val london = City.Values(c => Seq(c.id1 := 1, c.id2 := 1, c.name := "London"))
  val berlin = City.Values(c => Seq(c.id1 := 2, c.id2 := 1, c.name := "Berlin"))
  val paris = City.Values(c => Seq(c.id1 := 1, c.id2 := 2, c.name := "Paris"))
  val rome = City.Values(c => Seq(c.id1 := 2, c.id2 := 2, c.name := "Rome"))

  feature("existsAt") {

    scenario("single PK") {

      Person insert john
      Person existsAt 1 should be(true)
      Person existsAt 2 should be(false)
    }

    scenario("PK2") {

      City insertAll Seq(london, berlin, paris, rome)

      City existsAt (1, 1) should be(true)
      City existsAt (*, 1) should be(true)
      City existsAt (1, *) should be(true)
      City existsAt (*, *) should be(true)
      City existsAt (3, *) should be(false)
    }
  }

  feature("at") {

    scenario("single PK") {

      Person insert john
      Person at 1 select (_.name) option () should equal(Some("John"))
    }

    scenario("PK2") {

      City insertAll Seq(london, berlin, paris, rome)

      City at (1, 1) select (_.name) list () should equal(List("London"))
      City at (*, 1) select (_.name) list () should equal(List("London", "Berlin"))
      City at (1, *) select (_.name) list () should equal(List("London", "Paris"))
      City at (*, *) select (_.name) list () should equal(List("London", "Berlin", "Paris", "Rome"))
    }
  }

  feature("updateAt") {

    scenario("single PK") {

      Person insertAll Seq(john, tom)

      Person updateAt 1 set Person.Value(_.name := "Johnny")
      Person select (_.name) set () shouldEqual Set("Johnny", "Tom")
    }

    scenario("PK2") {

      City insertAll Seq(london, berlin, paris, rome)

      City updateAt (1, 1) set City.Value(_.name := "Copenhagen")
      City select (_.name) set () shouldEqual Set("Copenhagen", "Berlin", "Paris", "Rome")

      City deleteWhere (_ => True)
      City insertAll Seq(london, berlin, paris, rome)

      City updateAt (*, 1) set City.Value(_.name := "Madrid")
      City select (_.name) set () shouldEqual Set("Madrid", "Madrid", "Paris", "Rome")

      City deleteWhere (_ => True)
      City insertAll Seq(london, berlin, paris, rome)

      City updateAt (1, *) set City.Value(_.name := "Prague")
      City select (_.name) set () shouldEqual Set("Prague", "Berlin", "Prague", "Rome")

      City deleteWhere (_ => True)
      City insertAll Seq(london, berlin, paris, rome)

      City updateAt (*, *) set City.Value(_.name := "New York")
      City select (_.name) set () shouldEqual Set("New York", "New York", "New York", "New York")
    }
  }

  feature("deleteAt") {

    scenario("single PK") {

      Person insertAll Seq(john, tom)

      Person deleteAt 1
      Person select (_.name) list () should equal(List("Tom"))
    }

    scenario("PK2") {

      City insertAll Seq(london, berlin, paris, rome)

      City deleteAt (1, 1)
      City select (_.name) list () should equal(List("Berlin", "Paris", "Rome"))

      City deleteWhere (_ => True)
      City insertAll Seq(london, berlin, paris, rome)

      City deleteAt (1, *)
      City select (_.name) list () should equal(List("Berlin", "Rome"))

      City deleteWhere (_ => True)
      City insertAll Seq(london, berlin, paris, rome)

      City deleteAt (*, 1)
      City select (_.name) list () should equal(List("Paris", "Rome"))

      City deleteWhere (_ => True)
      City insertAll Seq(london, berlin, paris, rome)

      City deleteAt (*, *)
      City select (_.name) list () should equal(Nil)
    }
  }
}

