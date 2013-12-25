package org.shaqal.experiments

object Implicits2 {
  
  trait Foocator[A, B]
  
  trait Foo[A] {
    def foo[B](implicit f: Foocator[A, B]) = f
  }
  
  object IntFoo extends Foo[Int]
  
  trait Bar
  
  object Bar {
    implicit val intFoocator = new Foocator[Int, Bar] {}
  }
  
  IntFoo.foo[Bar]
  
  
  
  
  
}