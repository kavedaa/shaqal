package org.shaqal.test

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class MapperJoinTest extends AnyFunSuite with BeforeAndAfter with Matchers {

  object model {
    case class TableA(id: Int, value: String)
    case class TableB(id: Int, a: Option[TableA])
  }

  object DB extends Database with DefaultSchema {

    type D = TestDB

    trait TableAMapper extends Mapper[model.TableA] with TableDefinition {

      val id = new int("id") with notnull
      val value = new varchar(100)("value") with notnull

      val pk = id

      def fields = Seq(id, value)

      val (reader, writer) = RW(
        implicit rs => model.TableA(id.read, value.read),
        x => Seq(id := x.id, value := x.value))

      def constraints = Nil
    }

    object TableA extends Table("TableA") with TableAMapper

    object TableB extends Table("TableB") with Mapper[model.TableB] with TableDefinition {

      val id = new int("id") with notnull
      val aId = new int("aId") with nullable

      val a = new LeftJoin(aId) with MapperJoinNullable[model.TableA] with TableAMapper {
        def tableName = DB.TableA.tableName
      }

      def fields = Seq(id, a)

      val (reader, writer) = RW(
        implicit rs => model.TableB(id.read, a.read),
        x => Seq(id := x.id, aId := x.a map (_.id)))

      def constraints = Nil
    }
  }

  implicit def dbc: DBC[TestDB]

  before {
    DB.TableA.create()
    DB.TableB.create()
  }

  after {
    DB.TableA.drop(true)
    DB.TableB.drop(true)
  }

  import model._

  test("entry exists in foreign table") {

    DB.TableA insert TableA(1, "hello")
    DB.TableB insert TableB(1, Some(TableA(1, "hello")))

    val res = DB.TableB.option()

    res shouldEqual Some(TableB(1, Some(TableA(1, "hello"))))
  }
  
  test("no corresponding entry in foreign table") {

    DB.TableB insert TableB(1, Some(TableA(1, "hello")))

    val res = DB.TableB.option()

    res shouldEqual Some(TableB(1, None))
  }

  test("value of foreign key column is null") {

    DB.TableB insert TableB(1, None)

    val res = DB.TableB.option()

    res shouldEqual Some(TableB(1, None))
  }
  
}