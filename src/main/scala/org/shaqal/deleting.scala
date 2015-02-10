package org.shaqal

import org.shaqal._
import org.shaqal.sql._

trait Deleting { this: AccessorLike =>

  def deleteWhere(where: R => Expr)(implicit c: -:[D]) =
    c delete new DeleteSQL(r, where(r))
}