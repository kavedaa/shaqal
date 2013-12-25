package org.shaqal.test

import org.shaqal._
import org.shaqal.test.db.TestDB
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfter

abstract class AdhocJoiningTest extends FunSuite with ShouldMatchers with BeforeAndAfter {

  implicit def dbc: DBC[TestDB]

  trait Fields { this: TableLike =>

    val i = new int("i") with notnull
    val in = new int("in") with nullable
    val s = new varchar(100)("s") with notnull
    val sn = new varchar(100)("sn") with nullable

    def fields = Seq(i, in, s, sn)
  }

  trait FooTable extends Fields with Accessor with TableDefinition {
    def constraints = Nil
  }

  trait BarTable extends Fields with Accessor with TableDefinition {
    def constraints = Nil
  }

  object AdhocTestDB extends Database with DefaultSchema {

    type D = TestDB

    object FooTable extends Table("Foo") with FooTable

    object BarTable extends Table("Bar") with BarTable
  }

  import AdhocTestDB._

  before {

    FooTable drop true
    BarTable drop true
    FooTable createTable ()
    BarTable createTable ()
  }

  test("adhoc join") {

    FooTable insert FooTable.Values(f => Seq(f.i := 1, f.s := "one"))
    FooTable insert FooTable.Values(f => Seq(f.i := 2, f.s := "two"))
    FooTable insert FooTable.Values(f => Seq(f.i := 3, f.s := "three"))

    BarTable insert BarTable.Values(b => Seq(b.i := 2, b.s := "a"))
    BarTable insert BarTable.Values(b => Seq(b.i := 3, b.s := "b"))
    BarTable insert BarTable.Values(b => Seq(b.i := 4, b.s := "c"))

    val joined = FooTable join BarTable on { (foo, bar) => (foo.i -> bar.i) && (foo.i -> bar.i) }

    joined select { case (foo, bar) => (foo.s, bar.s) } list () should equal(List(("two", "a"), ("three", "b")))

  }

}