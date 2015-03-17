package org.shaqal

import org.shaqal._
import org.shaqal.sql._

trait Field

case class FlatForeign(path: Seq[TableLike], table: TableLike, joinType: JoinType, joinExpr: JoinExpr)

trait Fields { this: TableLike =>

  trait Foreign extends Field with ReadOnlyAccessorLike { f =>

    val schema = Fields.this.tableLike.schema
    val joinType: JoinType
    val fks: Seq[Col]
    def joinExpr: JoinExpr

    override def aliasPath = Fields.this.tableLike.aliasPath :+ tableLike    
  }

  def fields: Seq[Field]

  def foreigns = fields collect { case foreign: Foreign => foreign }

  //  def cols = (fields collect { case c: Col => c }) ++ (foreigns flatMap (_.fks))

  def cols = fields flatMap {
    case col: Col         => Seq(col)
    case foreign: Foreign => foreign.fks
  }

}