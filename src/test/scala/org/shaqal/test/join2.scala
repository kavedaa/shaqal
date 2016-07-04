package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class Join2Test extends FunSuite with BeforeAndAfter with Matchers {

  object model {
    case class Address(id1: Int, id2: Int, street: String)
    case class Person(id: Int, address: Address)
    case class PersonT(id: Int, addressId1: Int, addressId2: Int)
    case class NPerson(id: Int, address: Option[Address])
    case class NPersonT(id: Int, addressId1: Option[Int], addressId2: Option[Int])
  }

  object DB extends Database with DefaultSchema {

    type D = TestDB

    trait AddressMapper extends Mapper[model.Address] with TableDefinition {

      val id1 = new int("id1") with notnull
      val id2 = new int("id2") with notnull
      val street = new varchar(100)("street") with notnull

      def fields = Seq(id1, id2, street)

      val pk = (id1, id2)

      val (reader, writer) = RW(
        implicit rs => model.Address(id1.read, id2.read, street.read.trim),
        a => Seq(id1 := a.id1, id2 := a.id2, street := a.street))

      def constraints = Nil
    }

    object Address extends Table("Address") with AddressMapper

    object Person extends Table("Person") with DualMapper[model.Person, model.PersonT] with TableDefinition {

      val id = new int("id") with notnull
      val addressId1 = new int("addressId1") with notnull
      val addressId2 = new int("addressId2") with notnull

      val address = new Join2(addressId1, addressId2) with MapperJoin2[model.Address] with AddressMapper {
        def tableName = Address.tableName
      }

      def fields = Seq(id, address)

      val (reader, writer) = RW(
        implicit rs => model.Person(id.read, address.read),
        p => Seq(id := p.id, addressId1 := p.addressId1, addressId2 := p.addressId2))

      def constraints = Nil
    }

    object NPerson extends Table("NPerson") with DualMapper[model.NPerson, model.NPersonT] with TableDefinition {

      val id = new int("id") with notnull
      val addressId1 = new int("addressId1") with nullable
      val addressId2 = new int("addressId2") with nullable

      val address = new LeftJoin2(addressId1, addressId2) with MapperJoin2Nullable[model.Address] with AddressMapper {
        def tableName = Address.tableName
      }

      def fields = Seq(id, address)

      val (reader, writer) = RW(
        implicit rs => model.NPerson(id.read, address.read),
        p => Seq(id := p.id, addressId1 := p.addressId1, addressId2 := p.addressId2))

      def constraints = Nil
    }

  }

  implicit def dbc: DBC[TestDB]

  before {
    DB.Address create ()
    DB.Person create ()
    DB.NPerson create ()
  }

  after {
    DB.Person drop true
    DB.NPerson drop true
    DB.Address drop true
  }

  test("join") {

    DB.Address ++= Seq(model.Address(1, 1, "Strandgaten"), model.Address(1, 2, "Bryggen"))
    DB.Person ++= Seq(model.PersonT(1, 1, 1), model.PersonT(2, 1, 1), model.PersonT(3, 1, 2))

    DB.Person list () map (_.address.street) shouldEqual Seq("Strandgaten", "Strandgaten", "Bryggen")

  }

  test("nullable join") {

    DB.NPerson ++= Seq(model.NPersonT(1, Some(1), Some(1)), model.NPersonT(2, Some(1), None), model.NPersonT(3, Some(1), Some(2)))

    DB.NPerson list () map (_.address map (_.street)) shouldEqual (Seq(None, None, None))

    DB.Address ++= Seq(model.Address(1, 1, "Strandgaten"), model.Address(1, 2, "Bryggen"))

    DB.NPerson list () map (_.address map (_.street)) shouldEqual (Seq(Some("Strandgaten"), None, Some("Bryggen")))

  }

}