package org.shaqal

import org.shaqal.sql._

trait Updating extends Writing { this: AccessorLike =>

  def updateWhere(where: R => Expr) = new UpdateWhere(where(r))

  class UpdateWhere(whereExpr: Expr) {
    def set[A](a: A)(implicit writer: W[A], c: -:[D]) =
      c update UpdateSQL(TableName(r), writer(a), whereExpr)
  }
}

