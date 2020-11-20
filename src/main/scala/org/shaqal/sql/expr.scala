package org.shaqal.sql

import org.shaqal._
import org.shaqal.sql.Util._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

sealed abstract class Expr {

  def and(that: Expr): Expr = if (that == False) False else AndExpr(List(this, that))
  def &&(that: Expr) = and(that)

  def or(that: Expr): Expr = if (that == True) True else OrExpr(List(this, that))
  def ||(that: Expr) = or(that)

//  def render(implicit adapter: Adapter): String
//  def pp(implicit adapter: Adapter): Element
  def params: ParamsSeq
}

sealed trait Primitive extends Expr {
//  def pp(implicit adapter: Adapter) = render(adapter)
  def params = Nil
}

object True extends Primitive {

  override def and(that: Expr) = that
  override def or(that: Expr) = this

  def render(implicit adapter: Adapter) = "1 = 1".parens
}

object False extends Primitive {

  override def and(that: Expr) = this
  override def or(that: Expr) = that

  def render(implicit adapter: Adapter) = "1 = 0".parens
}

abstract class BooleanExpr(val op: String) extends Expr {
  val es: Seq[Expr]

//  def render(implicit adapter: Adapter) = es map { _ render (adapter) } mkString (" " + op + " ") parens
  def params = es flatMap (_.params)

  def containsComplex = (es collect { case e: BooleanExpr => e } ).nonEmpty

//  def pp(implicit adapter: Adapter) =
//    if (containsComplex)
//      Element mkIndentList (es.toList map { _ pp (adapter) }, op) parens
//    else render(adapter)
}

case class AndExpr(es: Seq[Expr]) extends BooleanExpr("and")

case class OrExpr(es: Seq[Expr]) extends BooleanExpr("or")

//abstract class BinExpr(op: String) extends Expr {
//  val left, right: Expr
//  def render(implicit adapter: Adapter) = List(left render(adapter), op, right render(adapter)) mkString " " parens
//  def pp(implicit adapter: Adapter) = render(adapter)
//  def params = left.params ++: right.params
//}

sealed abstract class ColumnExpr extends Expr {
  val column: Column
  val op: String
}

abstract class InfixColumnExpr(val op: String) extends ColumnExpr {
  val param: Param[_]
//  def render(implicit adapter: Adapter) = List(column.fullName, op, "(?)") mkString " "
  def params = param :: Nil
//  def pp(implicit adapter: Adapter) = render
}

case class Like(column: Column, param: Param[_]) extends InfixColumnExpr("like")
case class Eq(column: Column, param: Param[_]) extends InfixColumnExpr("=")
case class Ne(column: Column, param: Param[_]) extends InfixColumnExpr("!=")
case class Gt(column: Column, param: Param[_]) extends InfixColumnExpr(">")
case class Gte(column: Column, param: Param[_]) extends InfixColumnExpr(">=")
case class Lt(column: Column, param: Param[_]) extends InfixColumnExpr("<")
case class Lte(column: Column, param: Param[_]) extends InfixColumnExpr("<=")

abstract class ListInfixColumnExpr(val op: String) extends ColumnExpr {
//  def render(implicit adapter: Adapter) =
//    if (params nonEmpty)
//      List(column.fullName, op, (params map { _ => "(?)" } mkString ", ").parens) mkString " " parens
//    else
//      False render (adapter)
//  def pp(implicit adapter: Adapter) = render(adapter)
}

case class In(column: Column, values: Seq[Param[_]]) extends Expr {
  def params = values
}

abstract class PostfixColumnExpr(val op: String) extends ColumnExpr {
//  def render(implicit adapter: Adapter) = (column.fullName + " " + op).parens
  def params = Nil
//  def pp(implicit adapter: Adapter) = render(adapter)
}

case class IsNull(column: Column) extends PostfixColumnExpr("is null")
case class IsNotNull(column: Column) extends PostfixColumnExpr("is not null")

object Expr {

  def render(expr: Expr)(implicit columnFormat: ColumnFormat, adapter: Adapter): String = expr match {
    
    case True =>
      "1 = 1".parens
      
    case False =>
      "1 = 0".parens
      
    case b: BooleanExpr =>
      (b.es map render mkString (" " + b.op + " ")).parens
      
    case i: InfixColumnExpr =>
      Seq(i.column.render, i.op, "(?)") mkString " "
      
    case l: ListInfixColumnExpr =>
      if (l.params.nonEmpty)
        (Seq(l.column.render, l.op, (l.params map (_ => "(?)") mkString ", ").parens) mkString " ").parens
      else
        render(False)
        
    case p: PostfixColumnExpr =>
      (p.column.render + " " + p.op).parens
      
    case In(column, values) =>
      (column.render + " in " + (values map (_ => "(?)") mkString ", ").parens)
  }

  def pp(expr: Expr)(implicit columnFormat: ColumnFormat, adapter: Adapter): Element = expr match {
    case b: BooleanExpr if b.containsComplex =>
      (Element mkIndentList (b.es.toList map pp, b.op)).parens
    case _ =>
      render(expr)

  }
}
