package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet

trait Joining { r: ReadOnlyAccessorLike =>

  trait SingleJoinLike {
    val fk: Col //  this is the join column(s) on this table
    val pk: Col //  this is the join column(s) on the table that this will be joined with
    lazy val fks = Seq(fk)
    def joinExpr = new JoinExpr(fk, pk)
  }

  trait SingleJoin[C] extends SingleJoinLike {
    val pk: Col { type T = C }
    val fk: Col { type T = C }
  }

  trait AccessorJoin extends Foreign with SingleJoinLike

  abstract class Join[C, ColType <: Col](val fk: ColType { type T = C }) extends SingleJoin[C] {
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
  
}
