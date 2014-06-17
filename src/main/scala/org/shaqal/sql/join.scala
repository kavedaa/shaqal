package org.shaqal

import org.shaqal.sql._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

sealed abstract class JoinType(val keyword: String)

case object InnerJoin extends JoinType("inner join")
case object LeftJoin extends JoinType("left outer join")
case object RightJoin extends JoinType("right outer join")
case object FullJoin extends JoinType("full outer join")

abstract class JoinExpr extends Renderable { self =>
  def terms: Seq[JoinTerm]
  def &&(that: JoinTerm) = new JoinExpr { def terms = self.terms :+ that }
  def render(implicit adapter: Adapter) = terms map(_.render) mkString " and "
}

class JoinTerm(left: Column, right: Column) extends JoinExpr {
  def terms = Seq(this)
  override def render(implicit adapter: Adapter) = left.fullName + " = " + right.fullName  
}

object JoinExpr {
  def apply(ts: Seq[JoinTerm]) = new JoinExpr { def terms = ts }
}

object JoinTerm {
  implicit def joinTerm(t: (Column, Column)) = new JoinTerm(t._1, t._2)
}

class JoinElement(item: FromItem, joinType: JoinType, condition: JoinExpr) extends Renderable {
  def render(implicit adapter: Adapter) = List(joinType.keyword, item.render, "on", condition.render) mkString " "
  override def pp(implicit adapter: Adapter) = ElementList(joinType.keyword, Indent(item.pp), "on " + condition.render)
}

class JoinedItem(item: FromItem, joins: Seq[JoinElement]) extends FromItem {
  
  def tableAlias = None
  
  override def join(that: FromItem, joinType: JoinType, condition: JoinExpr)
    = new JoinedItem(item, joins :+ new JoinElement(that, joinType, condition))
    
  def render(implicit adapter: Adapter) = (item.render +: (joins map(_.render))) mkString " "
  override def pp(implicit adapter: Adapter) = ElementList(Indent(item.pp), joins map(_.pp) toList)
}
