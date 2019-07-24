package test

object Test {

  trait Foo {
    type F
  }

  trait Bar {

    type B

    val b: B

    def zoo[Z](z: B => Foo {type F = Z}) = z(b)
  }

  object Bar extends Bar {

    type B = this.type
    val b = this

    val i = new Foo {
      type F = Int
    }
  }

  Bar zoo (_.i)
}