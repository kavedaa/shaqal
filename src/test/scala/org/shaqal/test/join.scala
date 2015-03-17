package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class JoinTest extends FeatureSpec with BeforeAndAfter with Matchers {

  trait JoinTestForeignDefinition extends TableDefinition { this: TableLike with Fields =>

    val id = new int("id") with notnull
    val text = new varchar(500)("text") with notnull

    def fields = Seq(id, text)

    def constraints = Seq(PrimaryKey(id))
  }

  trait JoinTestForeignAccessor extends JoinTestForeignDefinition with Accessor with PK[Int] {
    val pk = id
  }

  case class JoinTestForeign(id: Int, text: String)

  trait JoinTestForeignMapper extends JoinTestForeignDefinition with PKMapper[JoinTestForeign, Int] {

    val (pk, pkf) = PK(id, _.id)

    val (reader, writer) = RW(
      implicit rs => JoinTestForeign(id.read, text.read),
      x => Seq(id := x.id, text := x.text))
  }

  type JoinTestTuple = (String, Int, Option[Int], Int, Option[Int])

  case class JoinTest(
    id: String,
    foreign1Id: Int,
    foreign2Id: Option[Int],
    foreign3: JoinTestForeign,
    foreign4: Option[JoinTestForeign])

  trait JoinTestMapper extends DualPKMapper[JoinTest, JoinTestTuple, String] with TableDefinition {

    val id = new char(1)("id") with notnull
    val foreign1Id = new int("foreignAccessorId") with notnull
    val foreign2Id = new int("foreignAccessorNullableId") with nullable
    val foreign3Id = new int("foreignMapperId") with notnull
    val foreign4Id = new int("foreignMapperNullableId") with nullable

    val foreign1 = new Join(foreign1Id) with AccessorJoin with JoinTestForeignAccessor {
      def tableName = DB.JoinTestForeign.tableName
    }

    val foreign2 = new Join(foreign2Id) with AccessorJoin with JoinTestForeignAccessor {
      def tableName = DB.JoinTestForeign.tableName
    }

    val foreign3 = new Join(foreign3Id) with MapperJoin[JoinTestForeign] with JoinTestForeignMapper {
      def tableName = DB.JoinTestForeign.tableName
    }

    val foreign4 = new Join(foreign4Id) with MapperJoin[JoinTestForeign] with JoinTestForeignMapper {
      def tableName = DB.JoinTestForeign.tableName
    }

    def fields = Seq(id, foreign1, foreign2, foreign3, foreign4)

    def constraints = Seq(
      ForeignKey(foreign1Id) references DB.JoinTestForeign(_.id),
      ForeignKey(foreign2Id) references DB.JoinTestForeign(_.id),
      ForeignKey(foreign3Id) references DB.JoinTestForeign(_.id),
      ForeignKey(foreign4Id) references DB.JoinTestForeign(_.id))

    val (pk, pkf) = PK(id, _._1)

    val (reader, writer) = RW(
      implicit rs => JoinTest(id.read, foreign1Id.read, foreign2Id.read, foreign3.read, foreign4.read),
      x => Seq(id := x._1, foreign1Id := x._2, foreign2Id := x._3, foreign3Id := x._4, foreign4Id := x._5))
  }

  object DB extends Database with DefaultSchema {
    type D = TestDB
    object JoinTestForeign extends Table("JoinTestForeign") with JoinTestForeignMapper
    object JoinTest extends Table("JoinTest") with JoinTestMapper

  }

  implicit def dbc: DBC[TestDB]

  before {

    DB.JoinTestForeign create ()
    DB.JoinTest create ()

    DB.JoinTestForeign ++= Seq(
      JoinTestForeign(1, "foo"),
      JoinTestForeign(2, "bar"),
      JoinTestForeign(3, "zip"))

    DB.JoinTest += ("a", 1, None: Option[Int], 2, Some(3): Option[Int])
  }

  after {
    DB.JoinTest drop (true)
    DB.JoinTestForeign drop (true)
  }

  feature("accessor join accessor") {

    scenario("single column join, not nullable") {

//      DB.JoinTest select (_.foreign1.text) option () should equal(Some("foo", "bar"))
      println(DB.JoinTest list())
    }

    scenario("single column join, nullable, when not null") {

      //    Person select (p => (p.name, p.occupation.name)) option () should equal(Some(("John", "Plumber")))
    }

    scenario("single column join, nullable, when null") {

      //    Person updateWhere(_ => True) set Person.Value(_.occupationId := None)

      //	We can't avoid null here...maybe with some hackery
      //    Person select (p => (p.name, p.occupation.name)) option () should equal(Some(("John", null)))
    }

  }

}