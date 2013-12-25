package org.shaqal.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfter
import org.scalatest.FeatureSpec
import java.sql.SQLException
import org.shaqal._
import org.shaqal.test.db.TestDB

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

trait ConstraintFKTestTableRelation extends Accessor with TableDefinition {
  val foreignId = new int("foreignId") with notnull
  def fields = Seq(foreignId)
  def constraints = Seq(ForeignKey(foreignId) references ConstraintsTestDB.ConstraintPKTestTable(_.id))
}

trait ConstraintFK2TestTableRelation extends Accessor with TableDefinition {
  val foreignId1 = new int("foreignId1") with notnull
  val foreignId2 = new int("foreignId2") with notnull
  def fields = Seq(foreignId1, foreignId2)
  def constraints = Seq(ForeignKey(foreignId1, foreignId2) references ConstraintsTestDB.ConstraintPK2TestTable(c => (c.id1, c.id2)))
}

trait ConstraintsTestDB extends Database with DefaultSchema {
  object ConstraintPKTestTable extends Table("ConstraintPKTestTable") with ConstraintPKTestTableRelation
  object ConstraintPK2TestTable extends Table("ConstraintPK2TestTable") with ConstraintPK2TestTableRelation
  object ConstraintFKTestTable extends Table("ConstraintFKTestTable") with ConstraintFKTestTableRelation
  object ConstraintFK2TestTable extends Table("ConstraintFK2TestTable") with ConstraintFK2TestTableRelation
}

object ConstraintsTestDB extends ConstraintsTestDB {
  type D = TestDB
}

abstract class ConstraintsTest extends FeatureSpec with BeforeAndAfter with ShouldMatchers {

  implicit def dbc: DBC[TestDB]

  import ConstraintsTestDB._
  
  before {

    ConstraintFK2TestTable drop (true)
    ConstraintFKTestTable drop (true)
    ConstraintPK2TestTable drop (true)
    ConstraintPKTestTable drop (true)

    ConstraintPKTestTable createTable ()
    ConstraintPKTestTable addConstraints ()
    ConstraintPK2TestTable createTable ()
    ConstraintPK2TestTable addConstraints ()
    ConstraintFKTestTable createTable ()
    ConstraintFKTestTable addConstraints ()
    ConstraintFK2TestTable createTable ()
    ConstraintFK2TestTable addConstraints ()
  }

  feature("primary key") {

    scenario("inserting a row with PK value same as existing") {

      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)

      evaluating {
        ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)
      } should produce[SQLException]
    }
  }

  feature("composite primary key") {

    scenario("inserting a row with PK values same as existing") {

      ConstraintPK2TestTable insert ConstraintPK2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))

      evaluating {
        ConstraintPK2TestTable insert ConstraintPK2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))
      } should produce[SQLException]
    }
  }

  feature("foreign key") {

    scenario("inserting a row with an FK value that does not exists in the foreign table") {
      evaluating {
        ConstraintFKTestTable insert ConstraintFKTestTable.Value(_.foreignId := 1)
      } should produce[SQLException]
    }

    scenario("deleting a row that has a column referenced from another table") {

      ConstraintPKTestTable insert ConstraintPKTestTable.Value(_.id := 1)
      ConstraintFKTestTable insert ConstraintFKTestTable.Value(_.foreignId := 1)

      evaluating {
        ConstraintPKTestTable deleteWhere (_.id is 1)
      } should produce[SQLException]
    }

  }

  feature("composite foreign key") {

    scenario("inserting a row with FK values that does not exists in the foreign table") {
      evaluating {
        ConstraintFK2TestTable insert ConstraintFK2TestTable.Values(c => Seq(c.foreignId1 := 1, c.foreignId2 := 1))
      } should produce[SQLException]
    }

    scenario("deleting a row that has columns referenced from another table") {

      ConstraintPK2TestTable insert ConstraintPK2TestTable.Values(c => Seq(c.id1 := 1, c.id2 := 1))
      ConstraintFK2TestTable insert ConstraintFK2TestTable.Values(c => Seq(c.foreignId1 := 1, c.foreignId2 := 1))

      evaluating {
        ConstraintPK2TestTable deleteWhere (c => (c.id1 is 1) && (c.id2 is 1))
      } should produce[SQLException]
    }
  }
}