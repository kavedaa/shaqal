//package org.shaqal.experiments
//
//trait Reading {
//
//  trait Reader[T]
//
//  type R[T] <: Reader[T]
//
//  def read[T](implicit r: R[T])
//}
//
//class Foo extends Reading {
//
//  trait FooReader[T] extends Reader[T]
//  type R[T] = FooReader[T]
//
//  object FooReader {
//    implicit def intReader = new FooReader[Int] {}
//    implicit def stringReader = new FooReader[String] {}
//  }
//
//}
//
//object Test {
//
//  val foo = new Foo
//
//  foo.read[Int]
//}