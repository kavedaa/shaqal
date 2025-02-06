package org.shaqal.test

import org.scalatest._
import org.scalatest.featurespec._
import org.scalatest.matchers.should._

import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class AggregateTest extends AnyFeatureSpec with Matchers with BeforeAndAfter {

  implicit def dbc: DBC[TestDB]

  trait NumberAccessor extends Accessor with TableDefinition {
    val number = new int("number") with nullable
    def fields = Seq(number)
    def constraints = Nil
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB
    object Number extends Table("Person") with NumberAccessor
  }
  
  before {
    DB.Number.createTable()
  }

  after {
    DB.Number drop true    
  }
  
  implicit val w: DB.Number.AccessorWriter[Int] = DB.Number.Writer[Int]((c, x) => Seq(c.number := Some(x)))

  feature("count") {

    scenario("empty table") {

      DB.Number.count() should equal(0)
    }

    scenario("no column specified, no predicate") {

      DB.Number insertAll Seq(1, 2, 3, 4, 5)

      DB.Number.count() should equal(5)
    }

    scenario("with predicate") {

      DB.Number insertAll Seq(1, 2, 3, 4, 5)

      DB.Number.where(_.number > 3).count() should equal(2)
    }

    scenario("column specified") {

      DB.Number insertAll Seq(1, 2, 3, 4, 5)

      DB.Number insert DB.Number.Value(_.number := None)

      DB.Number.count() should equal(6)

      //	(When column is specified NULL values will not be included in count by database.)
      DB.Number.count(_.number) should equal(5)
    }

  }
  
  feature("max") {
    
    scenario("empty table") {
      
//      DB.Number max(_.number) should equal(None)
      DB.Number.maxCompat(DB.Number.number) should equal(None)
    }
    
    scenario("no predicate") {
      
      DB.Number insertAll Seq(1, 2, 3, 4, 5)

      DB.Number.maxCompat(DB.Number.number) should equal(Some(5))
    }
    
    scenario("with predicate") {

      DB.Number insertAll Seq(1, 2, 3, 4, 5)

      DB.Number.where(_.number < 3) maxCompat(DB.Number.number) should equal(Some(2))            
    }
  }
}