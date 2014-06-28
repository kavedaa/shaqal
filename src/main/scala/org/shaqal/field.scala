package org.shaqal

import org.shaqal._
import org.shaqal.sql._

trait Field

trait Fields { this: TableLike =>

  trait Foreign extends Field with ReadOnlyAccessorLike {
    def schema = Fields.this.tableLike.schema
    val joinType: JoinType
    val fks: Seq[Col]
    def joinExpr: JoinExpr
  }

  def fields: Seq[Field]

  def foreigns = fields collect { case foreign: Foreign => foreign }

//  def cols = (fields collect { case c: Col => c }) ++ (foreigns flatMap (_.fks))

  def cols = fields flatMap {
    case col: Col => Seq(col)
    case foreign: Foreign => foreign.fks
  }
  
}