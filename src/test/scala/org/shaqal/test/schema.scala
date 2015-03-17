package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class SchemaTest extends FunSuite with Matchers with BeforeAndAfter {

  object DB extends Database {
    
    type D = TestDB
    
    object Foo extends Schema("Foo") with SchemaDefinition {
      
      trait AddressAccessor extends Accessor with TableDefinition {
        val id = new int("id") with notnull
        val street = new varchar(1000)("street") with notnull
        val city = new varchar(1000)("city") with notnull
        def fields = Seq(id, street, city)
        val pk = id
        def constraints = Seq(PrimaryKey(id))
      }
      
      object Address extends Table("Address") with AddressAccessor      
    }
    
    object Bar extends Schema("Bar") with SchemaDefinition {
      
      object Person extends Table("Person") with Accessor with TableDefinition {
        val id = new int("id") with notnull
        val addressId = new int("addressId") with notnull
        val address = new Join(addressId) with AccessorJoin with Foo.AddressAccessor {
          override lazy val schema = Foo.Address.schema    //  TODO figure a way to avoid this uglyness
          def tableName = Foo.Address.tableName
        }
        def fields = Seq(id, address)
        def constraints = Seq(PrimaryKey(id), ForeignKey(addressId) references Foo.Address(_.id))
      }
    }
  }
  
  implicit def dbc: DBC[TestDB]
  
  before {
    DB.Foo createSchema()
    DB.Foo.Address create()
    DB.Bar createSchema()
    DB.Bar.Person create()
  }
  
  after {
    DB.Bar.Person drop true
    DB.Bar drop true
    DB.Foo.Address drop true
    DB.Foo drop true
  }
  
  test("join between tables in different schemas") {
    
    import DB.Foo.Address
    import DB.Bar.Person
    
    Address insert Address.Values(a => Seq(a.id := 1, a.street := "Strandgaten", a.city := "Bergen"))
    Person insert Person.Values(p => Seq(p.id := 1, p.addressId := 1))
    
    Person select(_.address.city) option() shouldEqual Some("Bergen")
    
    
  }
  
  
}