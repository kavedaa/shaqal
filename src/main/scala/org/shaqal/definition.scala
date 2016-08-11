package org.shaqal

import org.shaqal._
import org.shaqal.sql._
import org.shaqal.sql.adapter.Adapter
import org.shaqal.sql.ColumnDefinitionElements._
import scala.collection.mutable.ListBuffer

trait TableDefinition extends Constraints { this: TableLike with Fields =>

  def max = DataLength.Max

  trait identity extends notnull { this: Col =>
    addElement(ColumnDefinitionElements.Identity)
  }

  trait unique { this: ColumnDefinition =>
    addElement(ColumnDefinitionElements.Unique)
  }

  trait default[C] { this: ColumnDefinition =>
    def defaultValue: Param[C]
    addElement(ColumnDefinitionElements.Default(defaultValue))
  }

  def create[U](f: String => U)(implicit c: -:[D]): Boolean = {
    val created = createTable()
    if (created) {
      addReferentialConstraints()
      f(path mkString ".")
    }
    created
  }

  def create()(implicit c: -:[D]): Boolean = {
    create(s => Unit)
  }

  def createTable()(implicit c: -:[D]) = {
    if (!database.tableExists(this)) {
      val sql = c.adapter createTableSql (this, cols map c.adapter.columnDefinitionSql)
      c execute sql
      addNonReferentialConstraints()
      true
    }
    else false
  }

  def addConstraints()(implicit c: -:[D]) {
    addNonReferentialConstraints()
    addReferentialConstraints()
  }

  def addReferentialConstraints()(implicit c: -:[D]) =
    referentialConstraints foreach addConstraint

  def addNonReferentialConstraints()(implicit c: -:[D]) = 
    nonReferentialConstraints foreach addConstraint

  def addConstraint(constraint: Constraint)(implicit c: -:[D]) = {
      val sql = c.adapter addConstraintSql (this, constraint)
      c execute sql    
  }
  
  //  def tableExists()(implicit c: -:[D]) = c.adapter tableExists this

  def drop(areYouSure: Boolean)(implicit c: -:[D]) =
    if (areYouSure) {
      val sql = c.adapter dropTableSql this
      c execute sql
    }

}

trait SchemaDefinition { this: Database#Schema =>

  protected def createSql(implicit adapter: Adapter) =
    adapter createSchemaSql name

  protected def dropSql(implicit adapter: Adapter) =
    adapter dropSchemaSql name

  def create[U](f: String => U)(implicit c: -:[D]) {
    if (!schemaExists()) {
      createSchema()
      f(name)
    }
  }

  def create()(implicit c: -:[D]) {
    create(s => Unit)
  }

  def createSchema()(implicit c: -:[D]) { c execute createSql(c.adapter) }

  def schemaExists()(implicit c: -:[D]) = c.adapter schemaExists this

  def drop(areYouSure: Boolean)(implicit c: -:[D]) { c execute dropSql(c.adapter) }
}