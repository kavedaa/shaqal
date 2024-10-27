package org.shaqal.experiments

trait Box {

  type B <: Box
 
  def fix(implicit fixer: Fixer[B]): Unit
}

trait ThingLike {
  type B <: Box
  val box2: Box { type B = ThingLike.this.B }
  def fix2(fixer: Fixer[B]) = box2 fix fixer
}

trait Fixer[B <: Box]

trait OtherThing { this: ThingLike =>
  
  trait That extends ThingLike {
    type B = OtherThing.this.B
    val box2: Box { type B = OtherThing.this.B } = OtherThing.this.box2
  }
  
}


trait Database {

  type D <: Database

  def tableExists(implicit c: -:[D]): Unit

}

trait SchemaLike {
  type D <: Database
  def database: Database { type D = SchemaLike.this.D }

  def tableExists(implicit c: -:[D]) = database.tableExists
}

trait -:[+D <: Database]




trait Foo {
  type F <: Foo 
}

trait BarLike {
  type F <: Foo
  val foo: Foo { type F = BarLike.this.F }
}

trait OtherBar { other: BarLike =>
  
  trait That extends BarLike {
    type F = OtherBar.this.F
    val foo: Foo { type F = OtherBar.this.F } = other.foo
  }
  
}


