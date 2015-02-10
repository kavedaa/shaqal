package org.shaqal.test

import org.scalatest._
import org.shaqal._

abstract class Join2Test extends FeatureSpec with BeforeAndAfter with Matchers {

  case class Address(id1: Int, id2: Int, street: String)
  case class Person(id: Int, address: Address)

  trait AddressMapper extends Mapper[Address] with TableDefinition {

    val id1 = new int("id1") with notnull
    val id2 = new int("id2") with notnull
    val street = new varchar(100)("street") with notnull

    val pk = (id1, id2)

    val (reader, writer) = RW(
      implicit rs => Address(id1.read, id2.read, street.read),
      a => Seq(id1 := a.id1, id2 := a.id2, street := a.street))

    def fields = Seq(id1, id2)

    def constraints = Nil
  }

  trait PersonMapper extends Mapper[Person] with TableDefinition {

    val id = new int("id") with notnull
    val addressId1 = new int("addressId1") with notnull
    val addressId2 = new int("addressId2") with notnull

    def addressTable: TableLike

    val address = new Join2(addressId1, addressId2) with MapperJoin2[Address] with AddressMapper {
      def tableName = addressTable.tableName
    }

    def fields = Seq(id, address)
    
    val (reader, writer) = RW(
      implicit rs => Person(id.read, address.read),
      p => Seq(id := p.id, addressId1 := p.address.id1, addressId2 := p.address.id2))
    
    def constraints = Nil
  }

}