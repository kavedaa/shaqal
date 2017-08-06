package org.shaqal.sql

import org.shaqal.sql.Util._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._
import scala.collection.mutable.ListBuffer

sealed abstract class DataLength {
  def render: Option[String]
}

object DataLength {

  case class Value(value: Int) extends DataLength {
    def render = Some(s"${value.toString}")
  }

  case object Max extends DataLength {
    def render = Some("max")
  }

  case object None extends DataLength {
    def render = scala.None
  }

  implicit def fromInt(i: Int) = Value(i)
}

trait ColumnDefinition extends ColumnLike {

  def sqlType: Int

  def dataTypeName(adapter: Adapter) = fullDataType(adapter dataType sqlType)

  val elements = ListBuffer[ColumnDefinitionElement[_]]()

  def addElement(element: ColumnDefinitionElement[_]) { elements += element }

  def fullDataType(typeName: String): String

  override def hasGeneratedValue = elements exists(_.hasGeneratedValue)

  def definitionSql(implicit adapter: Adapter) = adapter columnDefinitionSql this
}

abstract class ColumnDefinitionElement[C] {
  val hasGeneratedValue = false
  def render(implicit adapter: Adapter): String
  def params: ParamsSeq = Nil
}

object ColumnDefinitionElements {

  object NotNullable extends ColumnDefinitionElement[Nothing] {
    def render(implicit adapter: Adapter) = "not null"
  }

  object Nullable extends ColumnDefinitionElement[Nothing] {
    def render(implicit adapter: Adapter) = "null"
  }

  object Identity extends ColumnDefinitionElement[Nothing] {
    override val hasGeneratedValue = true
    def render(implicit adapter: Adapter) = adapter.identity
  }

  object Unique extends ColumnDefinitionElement[Nothing] {
    def render(implicit adapter: Adapter) = "unique"
  }

  case class Default[C](value: Param[C]) extends ColumnDefinitionElement[C] {
    override val hasGeneratedValue = true
    def render(implicit adapter: Adapter) = "default (?)"
    override def params = List(value)
  }

}

 
