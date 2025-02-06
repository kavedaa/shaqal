package org.shaqal.experiments

object type_constructor {

  trait Apple {    
    type T
    type F[X]
    type Q = F[T]
  }
  
  
  trait Banana[A1 <: Apple, A2 <: Apple] {
    
    // type T1 = A1#T
    // type T2 = A2#T
    
    // type F1[X] = A1#F[X]
    // type F2[X] = A2#F[X]
    
//    type F[X1, X2] = ???
    
  }
  
  def juice(apple1: Apple, apple2: Apple) = {
    
    
    
  }
  
  trait Value {
    type T
    type F[X]
    type Q = F[T]
    def value: T
    def output: Q
  }  
    
  trait NotNullableValue extends Value {
    type F[X] = X
  }
    
  trait NullableValue extends Value {
    type F[X] = Option[X]
  }
    
  trait Value2[V1 <: Value, V2 <: Value] {
    type F[X1, X2] = Nothing // ???
    def combine(value1: V1, value2: V2): F[V1, V2]
  }

}


