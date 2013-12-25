package org.shaqal

import org.shaqal._
import org.shaqal.sql._

trait Inserting extends Writing { this: AccessorLike =>

  //	SQL

  def insertSql(columnValues: Seq[ColumnParam]) = InsertSQL(TableName(Inserting.this), columnValues)

  def batchInsertSql(columns: Seq[Column], paramsSeqs: Seq[Seq[Param[_]]]) = InsertSQL(TableName(Inserting.this), columns, paramsSeqs)

  //	Insert API

  def insert[A](a: A)(implicit writer: W[A], c: -:[D]): Option[GG] =
    insertMethod(insertSql(writer(a)))

  def insertAll[A](as: Seq[A])(implicit writer: W[A], c: -:[D]): List[GG] = {
    val paramsSeq = as map writer
    paramsSeq.headOption map { paramsHead =>
      batchInsertMethod(batchInsertSql(paramsHead map (_.column), paramsSeq map (_ map (_.param))))
    } getOrElse Nil
  }

  //	Symbolic aliases  

  def +=[A](a: A)(implicit writer: W[A], c: -:[D]) = insert(a)

  def ++=[A](as: Seq[A])(implicit writer: W[A], c: -:[D]) = insertAll(as)
}
