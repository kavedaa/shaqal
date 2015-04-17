package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class JoinTest extends FeatureSpec with BeforeAndAfter with Matchers {

  object DB extends Database with DefaultSchema {

    type D = TestDB

    trait TableAAccessor extends Accessor with TableDefinition {
      val id = new int("id") with notnull
      val pk = id
      def fields = Seq(id)
      def constraints = Nil
    }

    object TableA extends Table("TableA") with TableAAccessor

    object TableB extends Table("TableB") with Accessor with TableDefinition {
      val id = new int("id") with notnull
      val aId = new int("aId1") with notnull
      val a = new LeftJoin(aId) with AccessorJoin with TableAAccessor {
        def tableName = TableA.tableName
      }
      def fields = Seq(id, a)
      def constraints = Nil
    }

    object TableC extends Table("TableC") with Accessor with TableDefinition {
      val id = new int("id") with notnull
      val aId1 = new int("aId1") with notnull
      val aId2 = new int("aId2") with notnull
      val a1 = new Join(aId1) with AccessorJoin with TableAAccessor { def tableName = TableA.tableName }
      val a2 = new Join(aId2) with AccessorJoin with TableAAccessor {
        def tableName = TableA.tableName
        override def aliasName = super.aliasName + "2"
      }
      def fields = Seq(id, a1, a2)
      def constraints = Nil
    }
  }

  implicit def dbc: DBC[TestDB]

  before {

    DB.TableA create ()
    DB.TableB create ()
    DB.TableC create ()
  }

  after {
    DB.TableA drop (true)
    DB.TableB drop (true)
    DB.TableC drop (true)
  }

  feature("join the same table several times") {

    scenario("inner join") {

      DB.TableA insert DB.TableA.Value(_.id := 1)
      DB.TableC insert DB.TableC.Values(c => Seq(c.id := 1, c.aId1 := 1, c.aId2 := 1))

      val id = DB.TableC where (_.id is 1) select (_.a1.id) option ()

      id shouldEqual Some(1)
    }

  }

  feature("there is no corresponding entry in the foreign table") {

    scenario("left join") {

        DB.TableB insert DB.TableB.Values(b => Seq(b.id := 1, b.aId := 1))
        
        val res = DB.TableB select(b => (b.id, b.a.id)) option()
   
        res shouldEqual Some((1, 0))  // note: an int value of null becomes 0
    }
  }

}