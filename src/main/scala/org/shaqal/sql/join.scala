package org.shaqal

import scala.language.implicitConversions

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
  implicit val cf: ColumnFormat = ColumnFormat.TableAlias
  def terms = Seq(this)
  override def render(implicit adapter: Adapter) = left.render + " = " + right.render
}

object JoinExpr {
  def apply(ts: Seq[JoinTerm]) = new JoinExpr { def terms = ts }
}

object JoinTerm {
  implicit def joinTerm(t: (Column, Column)): JoinTerm = new JoinTerm(t._1, t._2)
}

class JoinElement(item: FromItem, joinType: JoinType, condition: JoinExpr) {
  def render(implicit adapter: Adapter) = Seq(joinType.keyword, item.render, "on", condition.render) mkString " "
  def pp(implicit adapter: Adapter) = ElementList(joinType.keyword, Indent(item.pp), "on " + condition.render)
}

class JoinedItem(baseTable: TableLike, joins: Seq[JoinElement]) extends FromItem {
  
  def render(implicit adapter: Adapter) = (baseTable.fullNameAndAlias +: (joins map(_.render))) mkString " "
  override def pp(implicit adapter: Adapter) = ElementList(Indent(baseTable.fullNameAndAlias), (joins map(_.pp)).toList)
}
