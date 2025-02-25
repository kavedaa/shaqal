package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.adapter._

trait Constraints { this: TableLike & TableDefinition & Fields =>

  sealed trait Constraint extends SingleSQL {
    val name: Option[String]
    def constraintName(implicit adapter: Adapter): String
  }

  class PrimaryKey private (columns: Seq[Column], val name: Option[String] = None) extends Constraint {
    def defaultName = ("PK" +: underscoreName +: (columns map(_.columnName))) mkString "_"
    def constraintName(implicit adapter: Adapter) = adapter identifier (name getOrElse defaultName)
    def render(implicit adapter: Adapter) = s"constraint $constraintName primary key (${columns map (adapter identifier _.columnName) mkString ", "})"
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

  class Unique private (columns: Seq[Column], val name: Option[String] = None) extends Constraint {
    def defaultName = ("UQ" +: underscoreName +: (columns map(_.columnName))) mkString "_"
    def constraintName(implicit adapter: Adapter) = adapter identifier (name getOrElse defaultName)
    def render(implicit adapter: Adapter) = s"constraint $constraintName unique (${columns map (adapter identifier _.columnName) mkString ", "})"
    def params = Nil
  }

  object Unique {
    def apply(cs: Column*) = new Unique(cs)
    def apply(name: String, cs: Column*) = new Unique(cs, Some(name))
//    private def product(p: Product) = new Unique(p.productIterator.toSeq.asInstanceOf[Seq[Column]])
//    def apply(cs: (Column, Column)) = product(cs)
//    def apply(cs: (Column, Column, Column)) = product(cs)
//    def apply(cs: (Column, Column, Column, Column)) = product(cs)
//    def apply(cs: (Column, Column, Column, Column, Column)) = product(cs)
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
  
  class ReferentialConstraint (columns: Seq[Column], ref: TableColumns, val name: Option[String] = None) extends Constraint {

    def defaultName(implicit adapter: Adapter) = {
      val referencing = underscoreName +: (columns map(_.columnName))
      val referenced = ref.table.underscoreName +: (ref.columns map(_.columnName))
      ("FK" +: referencing ++: referenced) mkString "_"
    }
    
    def constraintName(implicit adapter: Adapter) = adapter identifier (name getOrElse defaultName)
    
    private var deleteAction: Option[ReferentialAction] = None
    private var updateAction: Option[ReferentialAction] = None
    def onDelete(refAction: ReferentialAction) = { deleteAction = Some(refAction); this }
    def onUpdate(refAction: ReferentialAction) = { updateAction = Some(refAction); this }

    def render(implicit adapter: Adapter) = {
      val keyRef = s"constraint $constraintName foreign key (${columns map (c => adapter identifier c.columnName) mkString ", "}) references ${ref.render}"
      val delAction = deleteAction map(a => s"on delete ${a.render}")
      val updAction = updateAction map(a => s"on delete ${a.render}")
      List(Some(keyRef), delAction, updAction).flatten mkString " "
    }      
      
    def params = Nil
  }

  class ForeignKey private (columns: Seq[Column], val name: Option[String] = None) {
    def references(refColumns: TableColumns) = new ReferentialConstraint(columns, refColumns, name)
  }

  object ForeignKey {
    def apply(columns: Column*) = new ForeignKey(columns)
    def apply(name: String, columns: Column*) = new ForeignKey(columns, Some(name))
  }

  def constraints: Seq[Constraint]
  
  def referentialConstraints = constraints filter(_.isInstanceOf[ReferentialConstraint])
  def nonReferentialConstraints = constraints filterNot(_.isInstanceOf[ReferentialConstraint])
}

case class TableColumns(table: TableLike, columns: Seq[Column]) {
  def render(implicit adapter: Adapter) = s"${table.fullName}(${columns map (c => adapter identifier c.columnName) mkString ", "})"
}

trait TableColumnator[R <: ReadOnlyAccessorLike, -Z] {
  def columnate(r: R, f: R => Z): TableColumns
}

object TableColumnator {

  implicit def singleColumnator[R <: ReadOnlyAccessorLike]: TableColumnator[R, Column] = new TableColumnator[R, Column] {
    def columnate(r: R, f: R => Column) =
      new TableColumns(r, Seq(f(r)))
  }

  implicit def tuple2Columnator[R <: ReadOnlyAccessorLike]: TableColumnator[R, (Column, Column)] = new TableColumnator[R, (Column, Column)] {
    def columnate(r: R, f: R => (Column, Column)) =
      new TableColumns(r, f(r).productIterator.toSeq.asInstanceOf[Seq[Column]])
  }

  implicit def seqColumnator[R <: ReadOnlyAccessorLike]: TableColumnator[R, Seq[Column]] = new TableColumnator[R, Seq[Column]] {
    def columnate(r: R, f: R => Seq[Column]) =
      new TableColumns(r, f(r))
  }

}
  
    


