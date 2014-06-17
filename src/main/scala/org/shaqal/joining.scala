package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet

trait Joining { r: ReadOnlyAccessorLike =>

  trait SingleJoinLike {
    val fk: Col //  this is the join column(s) on this table
    val pk: Col //  this is the join column(s) on the table that this will be joined with
    lazy val fks = Seq(fk)
    def joinExpr = new JoinTerm(fk, pk)
  }

  trait DualJoinLike {
    val fk1, fk2: Col
    val pk1, pk2: Col
    lazy val fks = Seq(fk1, fk2)
    def joinExpr = JoinExpr(Seq(new JoinTerm(fk1, pk1), new JoinTerm(fk2, pk2)))
  }
  
  trait SingleJoin[C] extends SingleJoinLike {
    val pk: Col { type T = C }
    val fk: Col { type T = C }
  }

  trait DualJoin[C1, C2] extends DualJoinLike {
    val fk1: Col { type T = C1 }
    val fk2: Col { type T = C2 }
    val pk1: Col { type T = C1 }
    val pk2: Col { type T = C2 }
  }
  
  trait AccessorJoin extends Foreign with SingleJoinLike

  trait AccessorJoin2 extends Foreign with DualJoinLike
  
  abstract class Join[C, ColType <: Col](val fk: ColType { type T = C }) extends SingleJoin[C] {
    val joinType = InnerJoin
  }

  abstract class Join2[C1, C2, ColType1 <: Col, ColType2 <: Col](val fk1: ColType1 { type T = C1 }, val fk2: ColType2 { type T = C2 }) extends DualJoin[C1, C2] {
    val joinType = InnerJoin
  }
  
  abstract class LeftJoin[C, ColType <: Col](val fk: ColType { type T = C }) extends SingleJoin[C] {
    val joinType = LeftJoin
  }

  abstract class RightJoin[C, ColType <: Col](val fk: ColType { type T = C }) extends SingleJoin[C] {
    val joinType = RightJoin
  }

  abstract class FullJoin[C, ColType <: Col](val fk: ColType { type T = C }) extends SingleJoin[C] {
    val joinType = FullJoin
  }
  
  trait MapperForeign[J] extends Foreign with Readable { this: ReadOnlyMapperLike[J] =>
    type T = J
    def get(implicit rs: ResultSet): J = reader(rs)
  }

  trait MapperJoin[J] extends MapperForeign[J] with SingleJoinLike { this: ReadOnlyMapperLike[J] =>
    type F[X] = fk.F[X]
    def checkNull(implicit rs: ResultSet) = fk.checkNull
    def f[X](x: => X)(implicit rs: ResultSet) = fk f x
  }
  
  trait MapperJoin2[J] extends MapperForeign[J] with DualJoinLike { this: ReadOnlyMapperLike[J] =>
  type F[X] = Nothing	// question is how to do this type computation...
  }
  
}
