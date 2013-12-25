package org.shaqal.test

import org.scalatest.FeatureSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfter
import org.shaqal._
import org.shaqal.test.db.TestDB
import java.text.SimpleDateFormat

abstract class WhereTest extends FeatureSpec with ShouldMatchers with BeforeAndAfter {

  implicit def dbc: DBC[TestDB]

  import org.shaqal.test.db.CommonTestDB.{ DataTypesTable => DT }

  before {
    DT createTable ()

    val df = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")

    DT insert DT.Values(d => Seq(
      d.smallintTest := 1,
      d.smallintTestNullable := Some(1),
      d.intTestNullable := Some(123456),
      d.intTest := 123456,
      d.bigintTest := 12345678901L,
      d.bigintTestNullable := Some(12345678901L),
      d.char1Test := 'a',
      d.char1TestNullable := Some('a'),
      d.charTest := "abc",
      d.charTestNullable := Some("abc"),
      d.varcharTest := "John Smith",
      d.varcharTestNullable := Some("John Smith"),
      d.doubleTest := 3.14,
      d.doubleTestNullable := Some(3.14),
      d.timestampTest := df parse "2012.12.31 23:59:59",
      d.timestampTestNullable := Some(df parse "2012.12.31 23:59:59"),
      d.numericTest := BigDecimal("123.45"),
      d.numericTestNullable := Some(BigDecimal("123.45"))))

    DT insert DT.Values(d => Seq(
      d.smallintTest := 5,
      d.intTest := 523456,
      d.bigintTest := 52345678901L,
      d.char1Test := 'b',
      d.charTest := "dcf",
      d.varcharTest := "Bob Jones",
      d.doubleTest := 2.7,
      d.timestampTest := df parse "2013.12.31 23:59:59",
      d.numericTest := BigDecimal("523.45")))
  }

  after {
    DT drop (true)
  }

  info("Test all supported 'where' predicates with all supported data types")

  feature("is") {

    scenario("smallint, not null") {
      DT where (_.smallintTest is 1) select (_.smallintTest) list () should equal(List(1))
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable is 1) select (_.smallintTestNullable) list () should equal(List(Some(1)))
    }

    scenario("int, not null") {
      DT where (_.intTest is 123456) select (_.intTest) list () should equal(List(123456))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable is 123456) select (_.intTestNullable) list () should equal(List(Some(123456)))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest is 12345678901L) select (_.bigintTest) list () should equal(List(12345678901L))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable is 12345678901L) select (_.bigintTestNullable) list () should equal(List(Some(12345678901L)))
    }

    scenario("char1, not nullable") {
      DT where (_.char1Test is 'a') select (_.char1Test) list () should equal(List('a'))
    }
    
    scenario("char1, nullable") {
      DT where(_.char1TestNullable is 'a') select(_.char1TestNullable) list() should equal(List(Some('a')))
    }
    
  }

  feature("isnt") {

    scenario("smallint, not null") {
      DT where (_.smallintTest isnt 1) select (_.smallintTest) list () should equal(List(5))
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable isnt 1) select (_.smallintTestNullable) list () should equal(Nil)
    }

    scenario("int, not null") {
      DT where (_.intTest isnt 123456) select (_.intTest) list () should equal(List(523456))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable isnt 123456) select (_.intTestNullable) list () should equal(Nil)
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest isnt 12345678901L) select (_.bigintTest) list () should equal(List(52345678901L))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable isnt 12345678901L) select (_.bigintTestNullable) list () should equal(Nil)
    }

    scenario("char1, not nullable") {
      DT where (_.char1Test isnt 'a') select (_.char1Test) list () should equal(List('b'))
    }
    
    scenario("char1, nullable") {
      DT where(_.char1TestNullable isnt 'a') select(_.char1TestNullable) list() should equal(Nil)
    }
  }

  feature("is null") {

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable.isNull) select (_.smallintTestNullable) list () should equal(List(None))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable.isNull) select (_.intTestNullable) list () should equal(List(None))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable.isNull) select (_.bigintTestNullable) list () should equal(List(None))
    }

    scenario("char1, nullable") {
      DT where(_.char1TestNullable.isNull) select(_.char1TestNullable) list() should equal(List(None))
    }
  }

  feature("is not null") {

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable.isNotNull) select (_.smallintTestNullable) list () should equal(List(Some(1)))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable.isNotNull) select (_.intTestNullable) list () should equal(List(Some(123456)))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable.isNotNull) select (_.bigintTestNullable) list () should equal(List(Some(12345678901L)))
    }
    
    scenario("char1, nullable") {
      DT where(_.char1TestNullable.isNotNull) select(_.char1TestNullable) list() should equal(List(Some('a')))
    }
    
  }

  feature(">") {

    scenario("smallint, not null") {
      DT where (_.smallintTest > 1) select (_.smallintTest) list () should equal(List(5))
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable > 1) select (_.smallintTestNullable) list () should equal(Nil)
    }

    scenario("int, not null") {
      DT where (_.intTest > 123456) select (_.intTest) list () should equal(List(523456))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable > 123456) select (_.intTestNullable) list () should equal(Nil)
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest > 12345678901L) select (_.bigintTest) list () should equal(List(52345678901L))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable > 12345678901L) select (_.bigintTestNullable) list () should equal(Nil)
    }
  }

  feature(">=") {

    scenario("smallint, not null") {
      DT where (_.smallintTest >= 1) select (_.smallintTest) list () should equal(List(1, 5))
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable >= 1) select (_.smallintTestNullable) list () should equal(List(Some(1)))
    }

    scenario("int, not null") {
      DT where (_.intTest >= 123456) select (_.intTest) list () should equal(List(123456, 523456))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable >= 123456) select (_.intTestNullable) list () should equal(List(Some(123456)))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest >= 12345678901L) select (_.bigintTest) list () should equal(List(12345678901L, 52345678901L))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable >= 12345678901L) select (_.bigintTestNullable) list () should equal(List(Some(12345678901L)))
    }

  }

  feature("<") {

    scenario("smallint, not null") {
      DT where (_.smallintTest < 5) select (_.smallintTest) list () should equal(List(1))
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable < 5) select (_.smallintTestNullable) list () should equal(List(Some(1)))
    }

    scenario("int, not null") {
      DT where (_.intTest < 523456) select (_.intTest) list () should equal(List(123456))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable < 523456) select (_.intTestNullable) list () should equal(List(Some(123456)))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest < 52345678901L) select (_.bigintTest) list () should equal(List(12345678901L))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable < 52345678901L) select (_.bigintTestNullable) list () should equal(List(Some(12345678901L)))
    }

  }

  feature("<=") {

    scenario("smallint, not null") {
      DT where (_.smallintTest <= 5) select (_.smallintTest) list () should equal(List(1, 5))
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable <= 5) select (_.smallintTestNullable) list () should equal(List(Some(1)))
    }

    scenario("int, not null") {
      DT where (_.intTest <= 523456) select (_.intTest) list () should equal(List(123456, 523456))
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable <= 523456) select (_.intTestNullable) list () should equal(List(Some(123456)))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest <= 52345678901L) select (_.bigintTest) list () should equal(List(12345678901L, 52345678901L))
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable <= 52345678901L) select (_.bigintTestNullable) list () should equal(List(Some(12345678901L)))
    }

  }

}