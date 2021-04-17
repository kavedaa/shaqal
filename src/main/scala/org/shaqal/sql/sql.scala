package org.shaqal.sql

import scala.language.implicitConversions

import org.shaqal._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._

abstract class Renderable {
  def render(implicit adapter: Adapter): String
  def pp(implicit adapter: Adapter): Element = render  
}

abstract class Params {
  def render: String
}

case class ParamsSeq(self: Seq[Param[?]]) extends Params {
  def render = self map (_.render) mkString ", "
}

object ParamsSeq {
  implicit def toSeq(p: ParamsSeq): Seq[Param[?]] = p.self.toSeq
  implicit def fromSeq(ps: Seq[_ <: Param[?]]): ParamsSeq = ParamsSeq(ps)
}

case class BatchParams(self: Seq[ParamsSeq]) extends Params {
  def render = self map (_.render) mkString "\n"
}

object BatchParams {
  implicit def toSeq(b: BatchParams): Seq[ParamsSeq] = b.self.toSeq
  implicit def fromSeq(ps: Seq[ParamsSeq]): BatchParams = BatchParams(ps)
}

abstract class SQL extends Renderable {
  def params: Params
}

abstract class SingleSQL extends SQL {
  def params: ParamsSeq
}

abstract class BatchSQL extends SQL {
  def params: BatchParams
}

object SQL {

  def apply(statement: String): SingleSQL = apply(statement, Nil)

  def apply(statement: String, ps: ParamsSeq): SingleSQL = new SingleSQL {
    def render(implicit adapter: Adapter) = statement
    def params = ps
  }

  def apply(statement: String, ps: Seq[ParamsSeq]): BatchSQL = new BatchSQL {
    def render(implicit adapter: Adapter) = statement
    def params = ps
  }

  implicit def fromString(statement: String): SingleSQL = apply(statement)
}


object Util {

  class StringW(s: String) {
    def parens = "(" + s + ")"
  }

  implicit def stringW(s: String): StringW = new StringW(s)
}
