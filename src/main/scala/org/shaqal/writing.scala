package org.shaqal

import org.shaqal.sql._

trait Writing { 
  
  type R <: AccessorLike
  val r: R

  case class Value(f: r.type => ColumnParam)
  case class Values(f: r.type => Seq[ColumnParam])


  type W[A] <: Writer[A]
  
  
}