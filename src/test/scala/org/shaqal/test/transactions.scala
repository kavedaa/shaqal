package org.shaqal.test

import org.shaqal._
import org.scalatest._
import org.shaqal.test.db.TestDB
import scala.util.Failure

abstract class TransactionsTest extends FeatureSpec with Matchers with BeforeAndAfter {

  trait TableTemplate extends Accessor with TableDefinition {

    val name = new varchar(100)("name") with notnull
    val age = new int("age") with notnull

    def fields = Seq(name, age)

    def constraints = Nil
  }

  object TransactionsDB extends Database with DefaultSchema {

    type D = TestDB

    object TableA extends Table("TableA") with TableTemplate
    object TableB extends Table("TableB") with TableTemplate
  }

  import TransactionsDB._

  implicit def dbc: Connector[TestDB]

  before {
    TableA create ()
    TableB create ()
  }

  after {
    TableA drop true
    TableB drop true
  }

  feature("auto transaction") {

    scenario("single") {

      autoTransaction { implicit txc: TXC[TestDB] =>
        TableA insert TableA.Values(a => Seq(a.name := "John", a.age := 24))
        throw new Exception
      }

      TableA.count() should equal(0)
    }

    scenario("nested") {

      val txOuter = autoTransaction { implicit txc: TXC[TestDB] =>

        val txInner = autoTransaction { implicit txc: TXC[TestDB] =>
          TableA insert TableA.Values(a => Seq(a.name := "John", a.age := 24))
          throw new Exception("inner")
        }

        TableB insert TableB.Values(a => Seq(a.name := "John", a.age := 24))

        throw new Exception("outer")
      }

      val Failure(ex) = txOuter
      ex.getMessage should equal("inner")
      
      TableA.count() should equal(0)
      TableB.count() should equal(0)
    }
  }
}

