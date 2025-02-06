package org.shaqal.test

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class InheritDBTest extends AnyFunSuite with Matchers with BeforeAndAfter {

  trait PersonAccessor extends Accessor with TableDefinition {
    val name = new varchar(100)("name") with notnull
    val age = new int("age") with notnull
    def fields = Seq(name, age)
    def constraints = Nil
  }
  
  
  trait DB extends TestDB with Database with DefaultSchema {
    object Person extends Table("Person") with PersonAccessor
  }  

  object DB extends DB {
    type D = DB
  }

  trait SubDB extends DB {  
    object OtherPerson extends Table("OtherPerson") with PersonAccessor
  }

  object SubDB extends SubDB { 
    type D = SubDB 
  }
  
  implicit def dbc: DBC[SubDB]

  info("It should be possible to inherit and extend a Database")
  info("and use a connector parameterized with the sub DB on the super DB.")
  info("That is, the connector should be covariant (which it is).")
  
  //	We don't really need to test insert and read here, compilation is sufficient.
  //	Still, it's nice to see that it works in practice...
  
  before {
    
    DB.Person.createTable()

    SubDB.Person.createTable()
    SubDB.OtherPerson.createTable()
  }

  after {
    DB.Person.drop(true)
    SubDB.Person.drop(true)
    SubDB.OtherPerson.drop(true)
  }
  
  test("base DB") {

    DB.Person insert DB.Person.Values(b => Seq(b.name := "Tom", b.age := 25))

    val names: List[String] = DB.Person.select(_.name).list()

    names should equal(List("Tom"))
  }

  test("inherited DB") {

    SubDB.Person insert SubDB.Person.Values(b => Seq(b.name := "Tom", b.age := 25))
    SubDB.OtherPerson insert SubDB.OtherPerson.Values(b => Seq(b.name := "John", b.age := 45))

    val names: List[String] = SubDB.Person.select(_.name).list()
    val otherNames: List[String] = SubDB.OtherPerson.select(_.name).list()
    
    names should equal(List("Tom")) 
    otherNames should equal(List("John")) 
  }
}