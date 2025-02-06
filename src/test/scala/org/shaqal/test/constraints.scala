package org.shaqal.test

import java.sql.SQLException

import org.scalatest._
import org.scalatest.featurespec._
import org.scalatest.matchers.should._


import org.shaqal._
import org.shaqal.test.db.TestDB
import org.shaqal.sql.True

trait ConstraintPKTestTableRelation extends Accessor with TableDefinition {
  val id = new int("id") with notnull
  def fields = Seq(id)
  def constraints = Seq(PrimaryKey(id))
}

trait ConstraintPK2TestTableRelation extends Accessor with TableDefinition {
  val id1 = new int("id1") with notnull
  val id2 = new int("id2") with notnull
  def fields = Seq(id1, id2)
  def constraints = Seq(PrimaryKey(id1, id2))
}

trait ConstraintUQTestTableRelation extends Accessor with TableDefinition {
  val id = new int("id") with notnull
  def fields = Seq(id)
  def constraints = Seq(Unique(id))
}

trait ConstraintUQ2TestTableRelation extends Accessor with TableDefinition {
  val id1 = new int("id1") with notnull
  val id2 = new int("id2") with notnull
  def fields = Seq(id1, id2)
  def constraints = Seq(Unique(id1, id2))
}

trait ConstraintFKTestTableRelation extends Accessor with TableDefinition {
  val foreignId = new int("foreignId") with notnull
  def fields = Seq(foreignId)
  def constraints = Seq(ForeignKey(foreignId) references ConstraintsTestDB.ConstraintPKTestTable(_.id))
}

trait ConstraintFK2TestTableRelation extends Accessor with TableDefinition {
  val foreignId1 = new int("foreignId1") with notnull
  val foreignId2 = new int("foreignId2") with notnull
  def fields = Seq(foreignId1, foreignId2)
  def constraints = Nil // Seq(ForeignKey(foreignId1, foreignId2) references ConstraintsTestDB.ConstraintPK2TestTable(c => (c.id1, c.id2)))
}

trait ConstraintsTestDB extends Database with DefaultSchema {
  object ConstraintPKTestTable extends Table("ConstraintPKTestTable") with ConstraintPKTestTableRelation
  object ConstraintPK2TestTable extends Table("ConstraintPK2TestTable") with ConstraintPK2TestTableRelation
  object ConstraintUQTestTable extends Table("ConstraintUQTestTable") with ConstraintUQTestTableRelation
  object ConstraintUQ2TestTable extends Table("ConstraintUQ2TestTable") with ConstraintUQ2TestTableRelation
  object ConstraintFKTestTable extends Table("ConstraintFKTestTable") with ConstraintFKTestTableRelation
  object ConstraintFK2TestTable extends Table("ConstraintFK2TestTable") with ConstraintFK2TestTableRelation
}

object ConstraintsTestDB extends ConstraintsTestDB {
  type D = TestDB
}

abstract class ConstraintsTest extends AnyFeatureSpec with BeforeAndAfter with Matchers {

  implicit def dbc: DBC[TestDB]

  import ConstraintsTestDB._

  before {

    ConstraintFK2TestTable.drop(true)
    ConstraintFKTestTable.drop(true)
    ConstraintPK2TestTable.drop(true)
    ConstraintPKTestTable.drop(true)
    ConstraintUQ2TestTable.drop(true)
    ConstraintUQTestTable.drop(true)

    ConstraintPKTestTable.createTable()
    ConstraintPKTestTable.addReferentialConstraints()
    ConstraintPK2TestTable.createTable()
    ConstraintPK2TestTable.addReferentialConstraints()
    ConstraintUQTestTable.create()
    ConstraintUQ2TestTable.create()
    ConstraintFKTestTable.createTable()
    ConstraintFKTestTable.addReferentialConstraints()
    ConstraintFK2TestTable.createTable()
    ConstraintFK2TestTable.addReferentialConstraints()
  }

  feature("primary key") {

    scenario("inserting a row with PK value same as existing") {

      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)

      intercept[SQLException] {
        ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)
      }
    }
  }

  feature("composite primary key") {

    scenario("inserting a row with PK values same as existing") {

      ConstraintPK2TestTable insert ConstraintPK2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))

      intercept[SQLException] {
        ConstraintPK2TestTable insert ConstraintPK2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))
      }
    }
  }

  feature("unique") {

    scenario("inserting a row with unique value same as existing") {

      ConstraintUQTestTable insert ConstraintUQTestTable.Value(_.id := 1)

      intercept[SQLException] {
        ConstraintUQTestTable insert ConstraintUQTestTable.Value(_.id := 1)
      }
    }
  }

  feature("composite unique") {

    scenario("inserting a row with unique values same as existing") {

      ConstraintUQ2TestTable insert ConstraintUQ2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))

      intercept[SQLException] {
        ConstraintUQ2TestTable insert ConstraintUQ2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))
      }
    }
  }

  feature("foreign key") {

    scenario("inserting a row with an FK value that does not exists in the foreign table") {
      intercept[SQLException] {
        ConstraintFKTestTable insert ConstraintFKTestTable.Value(_.foreignId := 1)
      }
    }

    scenario("deleting a row that has a column referenced from another table") {

      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)
      ConstraintFKTestTable insert ConstraintFKTestTable.Value(_.foreignId := 1)

      intercept[SQLException] {
        ConstraintPKTestTable deleteWhere (_.id is 1)
      }
    }

    scenario("deleting rows where some of them has columns referenced from another table") {

      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)
      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 2)
      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 3)
      ConstraintFKTestTable insert ConstraintFKTestTable.Value(_.foreignId := 2)

      intercept[SQLException] {
        ConstraintPKTestTable deleteWhere (_ => True)
      }

      ConstraintPKTestTable.count() shouldEqual 3
    }
  }

  feature("composite foreign key") {

    scenario("inserting a row with FK values that does not exists in the foreign table") {
      intercept[SQLException] {
        ConstraintFK2TestTable insert ConstraintFK2TestTable.Values(c => Seq(c.foreignId1 := 1, c.foreignId2 := 1))
      }
    }

    scenario("deleting a row that has columns referenced from another table") {

      ConstraintPK2TestTable insert ConstraintPK2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))
      ConstraintFK2TestTable insert ConstraintFK2TestTable.Values(c => Seq(c.foreignId1 := 1, c.foreignId2 := 1))

      intercept[SQLException] {
        ConstraintPK2TestTable deleteWhere (c => (c.id1 is 1) && (c.id2 is 1))
      }
    }
  }
}