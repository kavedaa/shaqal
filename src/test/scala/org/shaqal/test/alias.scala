package org.shaqal.test

import org.scalatest._
import org.shaqal._
import org.shaqal.test.db.TestDB

abstract class AliasTest extends FeatureSpec with Matchers with BeforeAndAfter with Inside with OptionValues {

  object DB extends Database with DefaultSchema {

    type D = TestDB

    trait FoodAccessor extends Accessor with TableDefinition {
      val id = new int("id") with notnull
      val name = new varchar(100)("name") with notnull
      def fields = Seq(id, name)
      val pk = id
      def constraints = Seq(PrimaryKey(id))
    }

    object Food extends Table("Food") with FoodAccessor

    trait PetAccessor extends Accessor with TableDefinition {
      val id = new int("id") with notnull
      val name = new varchar(100)("name") with notnull
      val favFoodId = new int("favFoodId") with notnull
      val favFood = new Join(favFoodId) with AccessorJoin with FoodAccessor {
        def tableName = Food.tableName
      }
      def fields = Seq(id, name, favFood)
      val pk = id
      def constraints = Seq(PrimaryKey(id), ForeignKey(favFoodId) references Food(_.id))
    }

    object Pet extends Table("Pet") with PetAccessor

    object Person extends Table("Person") with Accessor with TableDefinition {
      val id = new int("id") with notnull
      val name = new varchar(100)("name") with notnull
      val favFoodId = new int("favFoodId") with notnull
      val favPetId = new int("favPetId") with notnull
      val favFood = new Join(favFoodId) with AccessorJoin with FoodAccessor {
        def tableName = Food.tableName
      }
      val favPet = new Join(favPetId) with AccessorJoin with PetAccessor {
        def tableName = Pet.tableName
      }
      def fields = Seq(id, name, favFood, favPet)
      def constraints = Seq(
        PrimaryKey(id),
        ForeignKey(favFoodId) references Food(_.id),
        ForeignKey(favPetId) references Pet(_.id))
    }

  }

  implicit def dbc: DBC[TestDB]

  before {
    DB.Food create ()
    DB.Pet create ()
    DB.Person create ()
  }

  after {
    DB.Person drop true
    DB.Pet drop true
    DB.Food drop true
  }

  feature("join the same table twice") {

    scenario("direct join + join through intermediary table") {

      DB.Food insert DB.Food.Values(f => Seq(f.id := 1, f.name := "Meat"))
      DB.Pet insert DB.Pet.Values(p => Seq(p.id := 1, p.name := "Dog", p.favFoodId := 1))
      DB.Person insert DB.Person.Values(p => Seq(p.id := 1, p.name := "Bob", p.favFoodId := 1, p.favPetId := 1))

      val res = DB.Person select (p => (p.name, p.favFood.id, p.favPet.favFood.id)) option ()

      inside(res.value) {
        case (name, foodId, petFoodId) =>
          name shouldBe "Bob"
          foodId shouldBe 1
          petFoodId shouldBe 1
      }
    }

  }

}