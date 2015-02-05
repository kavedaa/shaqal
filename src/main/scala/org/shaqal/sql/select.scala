package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

abstract class SelectExpression extends Renderable

class ColumnSeq(self: Seq[Column]) extends SelectExpression {
 def render(implicit adapter: Adapter) = self map(_.render) mkString ", "  
 override def pp(implicit adapter: Adapter) = CommaLines(self map(_.render) toList)
}

object SelectExpression {
  implicit def columnSeq(cs: Seq[Column]) = new ColumnSeq(cs) 
}

abstract class FromItem {
  def tableAlias: Option[String]
  def join(that: FromItem, joinType: JoinType, condition: JoinExpr) = 
    new JoinedItem(this, List(new JoinElement(that, joinType, condition)))
  def render(implicit adapter: Adapter): String
  def pp(implicit adapter: Adapter): Element
}

case class TableName(table: TableLike) extends FromItem {
  def tableAlias = None
  def fullName(implicit adapter: Adapter) = table.fullName
  def render(implicit adapter: Adapter) = fullName
  def pp(implicit adapter: Adapter) = render
}

trait SelectLike extends SingleSQL {
  val selectExpression: SelectExpression
  val from: FromItem
  val where: Expr
  
  def render(implicit adapter: Adapter) = List(
    "select", 
    selectExpression.render, 
    "from", 
    from.render) :::
    (if (where == True) 
      Nil 
    else 
      List("where", where.render)) mkString " "
      
  def params = where.params
  
  val es: List[Element] = Nil
  
  override def pp(implicit adapter: Adapter) = ElementList(
    "select",
    Indent(selectExpression.pp),
    "from",
    Indent(from.pp)) ++ 
    (if (where != True)
      ElementList(
        "where", 
        Indent(where.pp))
    else
      ElementList())    
}

case class SelectSQL(selectExpression: SelectExpression, from: FromItem, where: Expr) extends SelectLike