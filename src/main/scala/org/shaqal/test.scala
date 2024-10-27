package org.shaqal

trait ConstraintPKTestTableRelation extends Accessor with TableDefinition {
  val id = new int("id") with notnull
  def fields = Seq(id)
  def constraints = Seq(PrimaryKey(id))
}

trait ConstraintFKTestTableRelation extends Accessor with TableDefinition {
  val foreignId = new int("foreignId") with notnull
  def fields = Seq(foreignId)
  def constraintPKTestTable: ConstraintPKTestTableRelation
  // def ref = constraintPKTestTable(_.id)
  // def fk = ForeignKey(foreignId) references ref
  // def constraints = Seq(fk)
}

trait ConstraintsTestDB extends Database with DefaultSchema {
  object ConstraintPKTestTable extends Table("ConstraintPKTestTable") with ConstraintPKTestTableRelation
}

object ConstraintsTestDB extends ConstraintsTestDB {
  type D = ConstraintsTestDB
}


class Bar

object Bar {
  implicit def myBar: Bar = new Bar
}

class Foo {
  val i = 1
  def apply[Z](f: this.type => Z)(implicit bar: Bar) = bar
}

trait Zip {
  val foo: Foo
  def bar = foo(_.i)
  def foo2: Foo
  def bar2 = foo2(_.i)
}
    

