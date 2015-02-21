package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class JoinOrderTest extends FunSuite with Matchers with BeforeAndAfter {

  object DB extends Database with DefaultSchema {

    type D = TestDB
    
    trait CountryAccessor extends Accessor with TableDefinition {
      val id = new int("id") with notnull
      val name = new varchar(1000)("name") with notnull
      def fields = Seq(id, name)
      val pk = id
      def constraints = Seq(PrimaryKey(id))
    }
    
    object Country extends Table("Country") with CountryAccessor
    
    trait AddressAccessor extends Accessor with TableDefinition {
      val id = new int("id") with notnull
      val street = new varchar(1000)("street") with notnull
      val countryId = new int("countryId") with notnull
      val country = new Join(countryId) with AccessorJoin with CountryAccessor {
        def tableName = Country.tableName
      }
      def fields = Seq(id, street, country)
      val pk = id
      def constraints = Seq(PrimaryKey(id), ForeignKey(countryId) references Country(_.id))
    }
    
    object Address extends Table("Address") with AddressAccessor
    
    object Person extends Table("Person") with Accessor with TableDefinition {
      val id = new int("id") with notnull
      val name = new varchar(1000)("name") with notnull
      val addressId = new int("addressId") with nullable //  the reason we need outer join
      val address = new LeftJoin(addressId) with AccessorJoin with AddressAccessor {
        def tableName = Address.tableName
      }
      def fields = Seq(id, name, address)
      def constraints = Seq(PrimaryKey(id), ForeignKey(addressId) references Address(_.id))
    }
  }
  
  implicit def dbc: DBC[TestDB]
  
  before {
    DB.Country create()
    DB.Address create()
    DB.Person create()
  }

  after {
    DB.Person drop true
    DB.Address drop true
    DB.Country drop true
  }
  
  test("ensure correct order of joins - needed for outer joins") {
    
//    DB.Country insert DB.Country.Values(c => Seq(c.id := 1, c.name := "Norway"))
//    DB.Address insert DB.Address.Values(a => Seq(a.id := 1, a.street := "Strandgaten", a.c))

    DB.Person insert DB.Person.Values(p => Seq(p.id := 1, p.name := "Bob", p.addressId := None))
    
    DB.Person select(_.name) option() shouldEqual Some("Bob")
  }
  
}