package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.adapter._

trait Constraints { this: TableLike with TableDefinition =>

  sealed trait Constraint extends SingleSQL

  class PrimaryKey private (columns: Seq[Column]) extends Constraint {
    def render(implicit adapter: Adapter) = s"primary key (${columns map (_.columnName) mkString ", "})"
    def params = Nil
  }

  object PrimaryKey {
    def apply(c: Column) = new PrimaryKey(Seq(c))
    private def product(p: Product) = new PrimaryKey(p.productIterator.toSeq.asInstanceOf[Seq[Column]])
    def apply(cs: (Column, Column)) = product(cs)
    def apply(cs: (Column, Column, Column)) = product(cs)
    def apply(cs: (Column, Column, Column, Column)) = product(cs)
    def apply(cs: (Column, Column, Column, Column, Column)) = product(cs)
  }

  sealed trait ReferentialAction {
    def render: String
  }
  
  object Restrict extends ReferentialAction {
    def render = "restrict"
  }

  object Cascade extends ReferentialAction {
    def render = "cascade"
  }
  
  class ReferentialConstraint(columns: Seq[Column], refColumns: TableColumns) extends Constraint {

    private var deleteAction: Option[ReferentialAction] = None
    private var updateAction: Option[ReferentialAction] = None
    def onDelete(refAction: ReferentialAction) = { deleteAction = Some(refAction); this }
    def onUpdate(refAction: ReferentialAction) = { updateAction = Some(refAction); this }

    def render(implicit adapter: Adapter) = {
      val keyRef = s"foreign key (${columns map (_.columnName) mkString ", "}) references ${refColumns.render}"
      val delAction = deleteAction map(a => s"on delete ${a.render}")
      val updAction = updateAction map(a => s"on delete ${a.render}")
      List(Some(keyRef), delAction, updAction).flatten mkString " "
    }      
      
    def params = Nil
  }

  case class ForeignKey(columns: Column*) {
    def references(refColumns: TableColumns) = new ReferentialConstraint(columns, refColumns)
  }

  def constraints: Seq[Constraint]
}

class TableColumns(table: TableLike, columns: Seq[Column]) {
  def render(implicit adapter: Adapter) = s"${table.tableName}(${columns map (_.columnName) mkString ", "})"
}

trait TableColumnator[R <: ReadOnlyAccessorLike, -Z] {
  def columnate(r: R, f: R => Z): TableColumns
}

object TableColumnator {

  implicit def singleColumnator[R <: ReadOnlyAccessorLike] = new TableColumnator[R, Column] {
    def columnate(r: R, f: R => Column) =
      new TableColumns(r, Seq(f(r)))
  }

  implicit def tuple2Columnator[R <: ReadOnlyAccessorLike] = new TableColumnator[R, (Column, Column)] {
    def columnate(r: R, f: R => (Column, Column)) =
      new TableColumns(r, f(r).productIterator.toSeq.asInstanceOf[Seq[Column]])
  }
}
  
    


