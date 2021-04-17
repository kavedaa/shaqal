package foo

import scala.language.implicitConversions

object Foo {

  class A
  
  object A {
    implicit def toB(a: A): B = new B(a)
  }
  
  class B(a: A) {
    def bar = 3
  }
  
  val a = new A
  
  a.bar
  
  class C[T]
  
  class D[T](val c: C[T]) extends AnyVal {
    def bar = 3
  }
  
  trait E
  
  object E {
    implicit def cToD[T](c: C[T]): D[T] = new D(c) 
  } 
  
  val c = new C[E]
  
  c.bar
  
}

object Bar {
  
  trait A {
    def x[B[_], T](implicit b: B[T]) = b
  }
  
  object A {
    implicit val s: String = "hello"
    
  }
  
  val a = new A {}
  
  
  
//  a.x[String]
  
  
  
}