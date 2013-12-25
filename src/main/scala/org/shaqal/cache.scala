package org.shaqal


//class CachedMapper[DD <: Database, A, C](val self: ReadOnlyPKMapper[A, C] { type D = DD } ) {
//
//  val cache = collection.mutable.Map[C, Option[A]]()
//
//  def apply(pk: C)(implicit c: -:[DD]): Option[A] =
//    cache get pk getOrElse {
//      val value = self(pk)
//      cache(pk) = value
//      value
//    }
//}
//
//class Cached2Mapper[DD <: Database, A, C1, C2](val self: PK2Mapper[A, C1, C2] { type D = DD } ) {
//
//  val cache = collection.mutable.Map[(C1, C2), Option[A]]()
//
//  def apply(pk: (C1, C2))(implicit c: -:[DD]): Option[A] =
//    cache get pk getOrElse {
//      val value = null // self(pk)
//      cache(pk) = value
//      value
//    }
//}