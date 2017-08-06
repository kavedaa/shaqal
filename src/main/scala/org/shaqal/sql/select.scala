package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

abstract class SelectExpression extends Renderable {
}

class ColumnSeq(self: Seq[Column]) extends SelectExpression {
  implicit val cf = ColumnFormat.Full
  def render(implicit adapter: Adapter) = self map (_.render) mkString ", "
  override def pp(implicit adapter: Adapter) = CommaLines((self map (_.render)).toList)
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

  def selectExpression: SelectExpression
  def fromItem: FromItem
  def whereExpr: Expr
  def forUpdate: Boolean
  def locks: Seq[Lock]

  implicit val cf = ColumnFormat.TableAlias

  val Select = "select"
  val From = "from"
  val With = "with"
  val Where = "where"
  val ForUpdate = "for update"

  def render(implicit adapter: Adapter) = {
    val selectFrom = Seq(Select, selectExpression.render, From, fromItem.render)
    val withLock = if (locks.nonEmpty) Seq(With, Lock render locks) else Nil
    val where = if (whereExpr != True) Seq(Where, Expr render whereExpr) else Nil 
    val updateHint = if (forUpdate) Seq(ForUpdate) else Nil
    (selectFrom ++: withLock ++: where ++: updateHint) mkString " "
  }

  def params = whereExpr.params

  val es: List[Element] = Nil

  override def pp(implicit adapter: Adapter) = {
    val selectFrom = ElementList(Select, Indent(selectExpression.pp), From, Indent(fromItem.pp))
    val withLock = if (locks.nonEmpty) ElementList(With, Indent(Lock render locks)) else ElementList()
    val where = if (whereExpr != True) ElementList(Where, Indent(Expr pp whereExpr)) else ElementList()
    val updateHint = if (forUpdate) ElementList(ForUpdate) else ElementList()
    ElementList(selectFrom, withLock, where, updateHint)   
  }
}

case class SelectSQL(selectExpression: SelectExpression, fromItem: FromItem, whereExpr: Expr, forUpdate: Boolean = false, locks: Seq[Lock] = Nil) extends SelectLike