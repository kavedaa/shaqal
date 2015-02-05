package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB
import java.text.SimpleDateFormat

abstract class WhereTest extends FeatureSpec with Matchers with BeforeAndAfter {

  implicit def dbc: DBC[TestDB]

  import org.shaqal.test.db.CommonTestDB.{ DataTypesTable => DT }

    val dtf = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
    val df = new SimpleDateFormat("yyyy.MM.dd")
    
    final val LastSecondOf2012 = dtf parse "2012.12.31 23:59:59"
    final val LastDayOf2012 = new java.sql.Date((df parse "2012.12.31").getTime) 
    final val FirstSecondOf2013 = dtf parse "2013.01.01 00:00:01"
    final val FirstDayOf2013 = new java.sql.Date((df parse "2013.01.01").getTime) 
    
  before {
    
    DT createTable ()

    DT insert DT.Values(d => Seq(
      d.id := 1,
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
      d.timestampTest := LastSecondOf2012,
      d.timestampTestNullable := Some(LastSecondOf2012),
      d.dateTest := LastDayOf2012,
      d.dateTestNullable := Some(LastDayOf2012),
      d.numericTest := BigDecimal("123.45"),
      d.numericTestNullable := Some(BigDecimal("123.45")),
      d.bitTest := true,
      d.bitTestNullable := Some(true)))

    DT insert DT.Values(d => Seq(
      d.id := 2,
      d.smallintTest := 5,
      d.intTest := 523456,
      d.bigintTest := 52345678901L,
      d.char1Test := 'b',
      d.charTest := "dcf",
      d.varcharTest := "Bob Jones",
      d.doubleTest := 2.71,
      d.timestampTest := FirstSecondOf2013,
      d.dateTest := FirstDayOf2013,
      d.numericTest := BigDecimal("523.45"),
      d.bitTest := false))
  }

  after {
    DT drop (true)
  }

  info("Test all supported 'where' predicates with all applicable data types")

  feature("is null") {

    //  testing one datatype should be sufficient here 
    
    scenario("int, nullable") {
      DT where (_.intTestNullable.isNull) select (_.id) list () shouldEqual List(2)
    }
  }

  feature("is not null") {

    //  and here
    
    scenario("int, nullable") {
      DT where (_.intTestNullable.isNotNull) select (_.id) list () shouldEqual List(1)
    }

  }

  feature("is") {

    scenario("smallint, not null") {
      DT where (_.smallintTest is (1: Short)) select (_.id) list () shouldEqual List(1)
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable is (1: Short)) select (_.id) list () shouldEqual List(1)
    }
    
    scenario("smallint, nullable, is null") {
      DT where(_.smallintTestNullable is None) select(_.id) list() shouldEqual List(2)
    }

    scenario("int, not null") {
      DT where (_.intTest is 123456) select (_.id) list () shouldEqual List(1)
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable is 123456) select (_.id) list () shouldEqual List(1)
    }

    scenario("int, nullable, is some") {
      DT where (_.intTestNullable is Some(123456)) select (_.id) list () shouldEqual List(1)
    }

    scenario("int, nullable, is null") {
      DT where (_.intTestNullable is None) select (_.id) list () shouldEqual List(2)
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest is 12345678901L) select (_.id) list () shouldEqual List(1)
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable is 12345678901L) select (_.id) list () shouldEqual List(1)
    }

    scenario("bigint, nullable, is null") {
      DT where (_.bigintTestNullable is None) select (_.id) list () shouldEqual List(2)
    }

    scenario("char1, not null") {
      DT where (_.char1Test is 'a') select (_.id) list () shouldEqual List(1)
    }

    scenario("char1, nullable") {
      DT where (_.char1TestNullable is 'a') select (_.id) list () shouldEqual List(1)
    }

    scenario("char1, nullable, is null") {
      DT where (_.char1TestNullable is None) select (_.id) list () shouldEqual List(2)
    }

    scenario("char, not null") {
      DT where(_.charTest is "abc") select(_.id) list() shouldEqual List(1)
    }
    
    scenario("char, nullable") {
      DT where(_.charTestNullable is "abc") select(_.id) list() shouldEqual List(1)
    }
    
    scenario("char, nullable, is null") {
      DT where(_.charTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("varchar, not null") {
      DT where(_.varcharTest is "John Smith") select(_.id) list() shouldEqual List(1)
    }
    
    scenario("varchar, nullable") {
      DT where(_.varcharTestNullable is "John Smith") select(_.id) list() shouldEqual List(1)      
    }
    
    scenario("varchar, nullable, is null") {
      DT where(_.varcharTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("double, not null") {
      DT where(_.doubleTest is 3.14) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("double, nullable") {
      DT where(_.doubleTestNullable is 3.14) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("double, nullable, is null") {
      DT where(_.doubleTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("timestamp, not null") {
      DT where(_.timestampTest is LastSecondOf2012) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("timestamp, nullable") {
      DT where(_.timestampTestNullable is LastSecondOf2012) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("timestamp, nullable, is null") {
      DT where(_.timestampTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("date, not null") {
      DT where(_.dateTest is LastDayOf2012) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("date, nullable") {
      DT where(_.dateTestNullable is LastDayOf2012) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("date, nullable, is null") {
      DT where(_.dateTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
   
    scenario("numeric, not null") {
      DT where(_.numericTest is BigDecimal("123.45")) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("numeric, nullable") {
      DT where(_.numericTestNullable is BigDecimal("123.45")) select(_.id) list() shouldEqual List(1)
    }

    scenario("numeric, nullable, is null") {
      DT where(_.numericTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
   
    scenario("bit, not null") {
      DT where(_.bitTest is true) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("bit, nullable") {
      DT where(_.bitTestNullable is true) select(_.id) list() shouldEqual List(1)
    }

    scenario("bit, nullable, is null") {
      DT where(_.bitTestNullable is None) select(_.id) list() shouldEqual List(2)
    }
    
  }

  feature("isnt") {

    scenario("smallint, not null") {
      DT where (_.smallintTest isnt (1: Short)) select (_.id) list () shouldEqual List(2)
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable isnt (1: Short)) select (_.id) list () shouldEqual Nil
    }

    scenario("smallint, nullable, isn't some") {
      DT where (_.smallintTestNullable isnt Some(1: Short)) select (_.id) list () shouldEqual List(2)
    }

    scenario("smallint, nullable, isn't null") {
      DT where (_.smallintTestNullable isnt None) select (_.id) list () shouldEqual List(1)
    }
    
    scenario("int, not null") {
      DT where (_.intTest isnt 123456) select (_.id) list () shouldEqual List(2)
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable isnt 123456) select (_.id) list () shouldEqual Nil
    }

    scenario("int, nullable, isn't some") {
      DT where (_.intTestNullable isnt Some(123456)) select (_.id) list () shouldEqual List(2)
    }

    scenario("int, nullable, isn't null") {
      DT where (_.intTestNullable isnt None) select (_.id) list () shouldEqual List(1)
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest isnt 12345678901L) select (_.id) list () shouldEqual List(2)
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable isnt 12345678901L) select (_.id) list () shouldEqual Nil
    }
    
    scenario("bigint, nullable, isn't some") {
      DT where(_.bigintTestNullable isnt Some(12345678901L)) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("bigint, nullable, isn't null") {
      DT where(_.bigintTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }

    scenario("char1, not null") {
      DT where (_.char1Test isnt 'a') select (_.id) list () shouldEqual List(2)
    }

    scenario("char1, nullable") {
      DT where (_.char1TestNullable isnt 'a') select (_.id) list () shouldEqual Nil
    }
    
    scenario("char1, nullable, isn't some") {
      DT where(_.char1TestNullable isnt Some('a')) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("char1, nullable, isn't null") {
      DT where(_.char1TestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("char, not null") {
      DT where (_.charTest isnt "abc") select (_.id) list () shouldEqual List(2)
    }

    scenario("char, nullable") {
      DT where (_.charTestNullable isnt "abc") select (_.id) list () shouldEqual Nil
    }
    
    scenario("char, nullable, isn't some") {
      DT where(_.charTestNullable isnt Some("abc")) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("char, nullable, isn't null") {
      DT where(_.charTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }

    scenario("varchar, not null") {
      DT where (_.varcharTest isnt "John Smith") select (_.id) list () shouldEqual List(2)
    }

    scenario("varchar, nullable") {
      DT where (_.varcharTestNullable isnt "John Smith") select (_.id) list () shouldEqual Nil
    }
    
    scenario("varchar, nullable, isn't some") {
      DT where(_.varcharTestNullable isnt Some("John Smith")) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("varchar, nullable, isn't null") {
      DT where(_.varcharTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }

    scenario("double, not null") {
      DT where (_.doubleTest isnt 3.14) select (_.id) list () shouldEqual List(2)
    }

    scenario("double, nullable") {
      DT where (_.doubleTestNullable isnt 3.14) select (_.id) list () shouldEqual Nil
    }
    
    scenario("double, nullable, isn't some") {
      DT where(_.doubleTestNullable isnt Some(3.14)) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("double, nullable, isn't null") {
      DT where(_.doubleTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("timestamp, not null") {
      DT where (_.timestampTest isnt LastSecondOf2012) select (_.id) list () shouldEqual List(2)
    }

    scenario("timestamp, nullable") {
      DT where (_.timestampTestNullable isnt LastSecondOf2012) select (_.id) list () shouldEqual Nil
    }
    
    scenario("timestamp, nullable, isn't some") {
      DT where(_.timestampTestNullable isnt Some(LastSecondOf2012)) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("timestamp, nullable, isn't null") {
      DT where(_.timestampTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }

    scenario("date, not null") {
      DT where (_.dateTest isnt LastDayOf2012) select (_.id) list () shouldEqual List(2)
    }

    scenario("date, nullable") {
      DT where (_.dateTestNullable isnt LastDayOf2012) select (_.id) list () shouldEqual Nil
    }
    
    scenario("date, nullable, isn't some") {
      DT where(_.dateTestNullable isnt Some(LastDayOf2012)) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("date, nullable, isn't null") {
      DT where(_.dateTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("numeric, not null") {
      DT where (_.numericTest isnt BigDecimal(123.45)) select (_.id) list () shouldEqual List(2)
    }

    scenario("numeric, nullable") {
      DT where (_.numericTestNullable isnt BigDecimal(123.45)) select (_.id) list () shouldEqual Nil
    }
    
    scenario("numeric, nullable, isn't some") {
      DT where(_.numericTestNullable isnt Some(BigDecimal(123.45))) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("numeric, nullable, isn't null") {
      DT where(_.numericTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }
    
    scenario("bit, not null") {
      DT where (_.bitTest isnt true) select (_.id) list () shouldEqual List(2)
    }

    scenario("bit, nullable") {
      DT where (_.bitTestNullable isnt true) select (_.id) list () shouldEqual Nil
    }
    
    scenario("bit, nullable, isn't some") {
      DT where(_.bitTestNullable isnt Some(true)) select(_.id) list() shouldEqual List(2)
    }
    
    scenario("bit, nullable, isn't null") {
      DT where(_.bitTestNullable isnt None) select(_.id) list() shouldEqual List(1)
    }
    
  }

  // TODO: double, timestamp, date, numeric
  
  
  feature(">") {

    scenario("smallint, not null") {
      DT where (_.smallintTest > 1) select (_.smallintTest) list () shouldEqual List(5)
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable > 1) select (_.smallintTestNullable) list () shouldEqual Nil
    }

    scenario("int, not null") {
      DT where (_.intTest > 123456) select (_.intTest) list () shouldEqual List(523456)
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable > 123456) select (_.intTestNullable) list () shouldEqual Nil
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest > 12345678901L) select (_.bigintTest) list () shouldEqual List(52345678901L)
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable > 12345678901L) select (_.bigintTestNullable) list () shouldEqual Nil
    }
  }

  feature(">=") {

    scenario("smallint, not null") {
      DT where (_.smallintTest >= 1) select (_.smallintTest) list () shouldEqual List(1, 5)
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable >= 1) select (_.smallintTestNullable) list () shouldEqual List(Some(1))
    }

    scenario("int, not null") {
      DT where (_.intTest >= 123456) select (_.intTest) list () shouldEqual List(123456, 523456)
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable >= 123456) select (_.intTestNullable) list () shouldEqual List(Some(123456))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest >= 12345678901L) select (_.bigintTest) list () shouldEqual List(12345678901L, 52345678901L)
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable >= 12345678901L) select (_.bigintTestNullable) list () shouldEqual List(Some(12345678901L))
    }

  }

  feature("<") {

    scenario("smallint, not null") {
      DT where (_.smallintTest < 5) select (_.smallintTest) list () shouldEqual List(1)
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable < 5) select (_.smallintTestNullable) list () shouldEqual List(Some(1))
    }

    scenario("int, not null") {
      DT where (_.intTest < 523456) select (_.intTest) list () shouldEqual List(123456)
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable < 523456) select (_.intTestNullable) list () shouldEqual List(Some(123456))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest < 52345678901L) select (_.bigintTest) list () shouldEqual List(12345678901L)
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable < 52345678901L) select (_.bigintTestNullable) list () shouldEqual List(Some(12345678901L))
    }

  }

  feature("<=") {

    scenario("smallint, not null") {
      DT where (_.smallintTest <= 5) select (_.smallintTest) list () shouldEqual List(1, 5)
    }

    scenario("smallint, nullable") {
      DT where (_.smallintTestNullable <= 5) select (_.smallintTestNullable) list () shouldEqual List(Some(1))
    }

    scenario("int, not null") {
      DT where (_.intTest <= 523456) select (_.intTest) list () shouldEqual List(123456, 523456)
    }

    scenario("int, nullable") {
      DT where (_.intTestNullable <= 523456) select (_.intTestNullable) list () shouldEqual List(Some(123456))
    }

    scenario("bigint, not null") {
      DT where (_.bigintTest <= 52345678901L) select (_.bigintTest) list () shouldEqual List(12345678901L, 52345678901L)
    }

    scenario("bigint, nullable") {
      DT where (_.bigintTestNullable <= 52345678901L) select (_.bigintTestNullable) list () shouldEqual List(Some(12345678901L))
    }

  }

}