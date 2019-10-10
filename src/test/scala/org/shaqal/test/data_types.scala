package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.sql._
import org.shaqal.test.db.TestDB
import java.text.SimpleDateFormat

abstract class DataTypesTest extends FeatureSpec with BeforeAndAfter with Matchers {

  implicit def dbc: DBC[TestDB]

  import org.shaqal.test.db.CommonTestDB.{ DataTypesTable => DT }

  info("Test all supported datatypes - insert and read back data")

  val dtf = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
  val df = new SimpleDateFormat("yyyy.MM.dd")

  before {
    DT create ()

    DT insert DT.Values(d => Seq(
      d.id := 1,
      d.smallintTest := 1,
      d.intTest := 123456,
      d.bigintTest := 12345678901L,
      d.char1Test := 'a',
      d.charTest := "abc",
      d.varcharTest := "John Smith",
      d.doubleTest := 3.14,
      d.timestampTest := dtf parse "2012.12.31 23:59:59",
      d.dateTest := new java.sql.Date((df parse "2012.12.31").getTime),
      d.numericTest := BigDecimal("123.45"),
      d.bitTest := true))
  }

  after {
    DT drop true
  }

  feature("smallint") {

    scenario("not nullable") {
      DT select (_.smallintTest) option () should equal(Some(1))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.smallintTestNullable := Some(1))
      DT select (_.smallintTestNullable) option () should equal(Some(Some(1)))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.smallintTestNullable := None)
      DT select (_.smallintTestNullable) option () should equal(Some(None))
    }
  }

  feature("int") {

    scenario("not nullable") {
      DT select (_.intTest) option () should equal(Some(123456))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.intTestNullable := Some(123456))
      DT select (_.intTestNullable) option () should equal(Some(Some(123456)))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.intTestNullable := None)
      DT select (_.intTestNullable) option () should equal(Some(None))
    }
  }

  feature("bigint") {

    scenario("not nullable") {
      DT select (_.bigintTest) option () should equal(Some(12345678901L))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.bigintTestNullable := Some(12345678901L))
      DT select (_.bigintTestNullable) option () should equal(Some(Some(12345678901L)))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.bigintTestNullable := None)
      DT select (_.bigintTestNullable) option () should equal(Some(None))
    }
  }

  feature("char1") {

    scenario("not nullable") {
      DT select (_.char1Test) option () should equal(Some('a'))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.char1TestNullable := Some('a'))
      DT select (_.char1TestNullable) option () should equal(Some(Some('a')))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.char1TestNullable := None)
      DT select (_.char1TestNullable) option () should equal(Some(None))
    }
  }

  feature("char") {

    scenario("not nullable") {
      DT select (_.charTest) option () should equal(Some("abc"))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.charTestNullable := Some("abc"))
      DT select (_.charTestNullable) option () should equal(Some(Some("abc")))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.charTestNullable := None)
      DT select (_.charTestNullable) option () should equal(Some(None))
    }
  }

  feature("varchar") {

    scenario("not nullable") {
      DT select (_.varcharTest) option () should equal(Some("John Smith"))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.varcharTestNullable := Some("John Smith"))
      DT select (_.varcharTestNullable) option () should equal(Some(Some("John Smith")))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.varcharTestNullable := None)
      DT select (_.varcharTestNullable) option () should equal(Some(None))
    }

  }

  feature("double") {

    scenario("not nullable") {
      DT select (_.doubleTest) option () should equal(Some(3.14))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.doubleTestNullable := Some(2.78))
      DT select (_.doubleTestNullable) option () should equal(Some(Some(2.78)))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.doubleTestNullable := None)
      DT select (_.doubleTestNullable) option () should equal(Some(None))
    }

  }

  feature("timestamp") {

    scenario("not nullable") {
      DT select (_.timestampTest) option () map dtf.format should equal(Some("2012.12.31 23:59:59"))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.timestampTestNullable := Some(dtf parse "2012.12.31 23:59:59"))
      DT select (_.timestampTestNullable) option () map (_ map dtf.format) should equal(Some(Some("2012.12.31 23:59:59")))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.timestampTestNullable := None)
      DT select (_.timestampTestNullable) option () map (_ map dtf.format) should equal(Some(None))
    }

  }

  feature("date") {

    scenario("not nullable") {
      DT select (_.dateTest) option () map df.format should equal(Some("2012.12.31"))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.dateTestNullable := Some(new java.sql.Date((df parse "2012.12.31").getTime)))
      DT select (_.dateTestNullable) option () map (_ map df.format) should equal(Some(Some("2012.12.31")))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.dateTestNullable := None)
      DT select (_.dateTestNullable) option () map (_ map df.format) should equal(Some(None))
    }

  }

  feature("numeric") {

    scenario("not nullable") {
      DT select (_.numericTest) option () should equal(Some(BigDecimal("123.45")))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.numericTestNullable := Some(BigDecimal("123.45")))
      DT select (_.numericTestNullable) option () should equal(Some(Some(BigDecimal("123.45"))))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.numericTestNullable := None)
      DT select (_.numericTestNullable) option () should equal(Some(None))
    }
  }

  feature("bit") {

    scenario("not nullable") {
      DT select (_.bitTest) option () should equal(Some(true))
    }

    scenario("nullable, when not null") {
      DT updateWhere (_ => True) set DT.Value(_.bitTestNullable := Some(true))
      DT select (_.bitTestNullable) option () should equal(Some(Some(true)))
    }

    scenario("nullable, when null") {
      DT updateWhere (_ => True) set DT.Value(_.bitTestNullable := None)
      DT select (_.bitTestNullable) option () should equal(Some(None))
    }
  }
  
}