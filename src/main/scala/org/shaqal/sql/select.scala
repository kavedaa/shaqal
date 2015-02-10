package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

abstract class SelectExpression extends Renderable {
}

class ColumnSeq(self: Seq[Column]) extends SelectExpression {
  implicit val cf = ColumnFormat.Full  
 def render(implicit adapter: Adapter) = self map(_.render) mkString ", "  
 override def pp(implicit adapter: Adapter) = CommaLines(self map(_.render) toList)
}

object SelectExpression {
  implicit def columnSeq(cs: Seq[Column]) = new ColumnSeq(cs) 
}

abstract class FromItem {
  def render(implicit adapter: Adapter): String
  def pp(implicit adapter: Adapter): Element
}

case class TableName(table: TableLike) extends FromItem {
  def render(implicit adapter: Adapter) = table.fullNameAndAlias
  def pp(implicit adapter: Adapter) = render
}

trait SelectLike extends SingleSQL {
  
  val selectExpression: SelectExpression
  val from: FromItem
  val where: Expr
  
  implicit val cf = ColumnFormat.TableAlias
  
  def render(implicit adapter: Adapter) = List(
    "select", 
    selectExpression.render, 
    "from", 
    from.render) :::
    (if (where == True) 
      Nil 
    else 
      List("where", Expr render where)) mkString " "
      
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
        Indent(Expr pp where))
    else
      ElementList())    
}

case class SelectSQL(selectExpression: SelectExpression, from: FromItem, where: Expr) extends SelectLike