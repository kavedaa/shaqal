package org.shaqal.sql

import org.shaqal.sql.Util._
import org.shaqal.sql.adapter._
import org.shaqal.sql.pretty._
import scala.collection.mutable.ListBuffer

class DataLength(val value: Option[Int]) extends AnyVal

object DataLength {
  implicit def fromInt(i: Int) = new DataLength(Some(i))
}

trait ColumnDefinition extends ColumnLike {

  def sqlType: Int
  
  def dataTypeName(adapter: Adapter) = fullDataType(adapter dataType sqlType)
  
  //  val dataLength: ColumnDefinition.DataLength
  val elements = ListBuffer[ColumnDefinitionElement[_]]()

  def addElement(element: ColumnDefinitionElement[_]) { elements += element }

  //  def dataType = List(Some(dataTypeName), dataLength.value map ("(" + _ + ")")).flatten mkString

  def fullDataType(typeName: String): String

  override def hasGeneratedValue = elements filter (_.hasGeneratedValue) nonEmpty

  def definitionSql(implicit adapter: Adapter) = adapter columnDefinitionSql this
}

//trait NoColumnDefinition extends ColumnDefinition {
//  val dataLength = None
//  val elements = Nil
//}
//
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

 
