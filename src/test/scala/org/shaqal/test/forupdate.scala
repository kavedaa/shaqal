package org.shaqal.test

import org.shaqal._
import org.scalatest._
import org.shaqal.test.db.TestDB
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

abstract class ForUpdateTest extends FunSuite with Matchers with BeforeAndAfter {

  object DB extends Database with DefaultSchema {

    type D = TestDB

    object TableA extends Table("TableA") with Accessor with TableDefinition {

      val id = new int("id") with notnull
      val name = new varchar(100)("name") with notnull

      def fields = Seq(id, name)

      def constraints = Nil
    }

  }

  implicit def dbc: Connector[TestDB]

  before {
    DB.TableA drop true
    DB.TableA createTable ()
    DB.TableA insert DB.TableA.Values(a => Seq(a.id := 1, a.name := "John"))
  }

  after {
  }

  test("select") {

    Future {
      DB autoTransaction { implicit txc: TXC[TestDB] =>
        val a = DB.TableA where (_.id is 1) select (_.name) option ()
        Thread sleep 1000
        DB.TableA updateWhere (_.id is 1) set DB.TableA.Value(_.name := "Tom")
      }
    }

    //  We need to give the future some time to start run its code.
    Thread sleep 300

    val b = DB.TableA where (_.id is 1) select (_.name) option ()

    b shouldEqual Some("John")
  }

  test("select for update") {

    Future {
      DB autoTransaction { implicit txc: TXC[TestDB] =>
        val a = DB.TableA where (_.id is 1) selectForUpdate (_.name) option ()
        Thread sleep 2000
        DB.TableA updateWhere (_.id is 1) set DB.TableA.Value(_.name := "Tom")
      }
    }

    //  We need to give the future some time to start run its code.
    Thread sleep 300

    val b = DB.TableA where (_.id is 1) selectForUpdate (_.name) option ()

    b shouldEqual Some("Tom")
  }

}
