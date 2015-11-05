package org.shaqal

import org.shaqal.sql._
import scala.collection.mutable.ListBuffer
import java.sql._
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

trait DBOperations { this: Connector[_] =>

  def execute(sql: SingleSQL) {
    onSql(sql)
    try {
      val conn = getConnection
      try {
        val prep = conn prepareStatement sql.render
        try {
          setParams(prep, sql.params)
          prep execute ()
        }
        finally { prep close () }
      }
      finally { close(conn) }
    }
    catch {
      case ex: SQLException =>
        onError(sql, ex)
        throw ex
    }
  }

  //  def query[T](sql: SingleSQL, rsMapper: ResultSet => T): T =
  //    queryList(sql, rsMapper).headOption getOrElse { throw new NoSuchElementException("Because the result set was empty.") }
  //
  //  def queryOption[T](sql: SingleSQL, rsMapper: ResultSet => T): Option[T] =
  //    queryList(sql, rsMapper).headOption

  def queryElement[T](sql: SingleSQL, rsMapper: ResultSet => T): Option[T] = {
    queryColl(sql, rsMapper, Seq.newBuilder[T]).headOption
  }

  def queryColl[T, Coll[_]](sql: SingleSQL, rsMapper: ResultSet => T, builder: Builder[T, Coll[T]]): Coll[T] = {
    query(sql, rsMapper, (x: T) => builder += x)
    builder.result
  }

  def query[U, T](sql: SingleSQL, rsMapper: ResultSet => T, process: T => U) {
    onSql(sql)
    Debug incQueries ()
    try {
      val conn = getConnection
      try {
        val prep = conn prepareStatement sql.render
        try {
          setParams(prep, sql.params)
          val rs = prep executeQuery ()
          try {
            while (rs.next()) {
              Debug incRows ()
              process(rsMapper(rs))
            }
          }
          catch { case _: CancelException => }
          finally { rs close () }
        }
        finally { prep close () }
      }
      finally { close(conn) }
    }
    catch {
      case ex: SQLException =>
        onError(sql, ex)
        if (throwOnQuery) throw ex else Nil
    }
  }

  def insert[A](sql: SingleSQL, autos: Seq[String], autosMapper: Option[ResultSet => A] = None): Option[A] = {
    onSql(sql)
    try {
      val conn = getConnection
      try {
        //  TODO jtds (at least) throw an exception when there are more than one auto columns
        //  perhaps better to always just have one
        val prep = if (autosMapper.isDefined) conn.prepareStatement(sql.render, autos.toArray)
        else conn.prepareStatement(sql.render)
        try {
          setParams(prep, sql.params)
          if (prep.executeUpdate() > 0) autosMapper flatMap { mapper =>
            val rs = prep.getGeneratedKeys
            try {
              if (rs.next()) Some(mapper(rs))
              else None
            }
            finally { rs.close() }
          }
          else None
        }
        finally { prep.close() }
      }
      finally { close(conn) }
    }
    catch {
      case ex: SQLException =>
        onError(sql, ex)
        if (throwOnInsert) throw ex else None
    }
  }

  def batchInsert[A](batch: BatchSQL, autos: Seq[String], autosMapper: Option[ResultSet => A] = None): List[A] = {
    onSql(batch)
    try {
      val conn = getConnection
      try {
        val prep = if (autosMapper.isDefined) conn.prepareStatement(batch.render, autos.toArray)
        else conn.prepareStatement(batch.render)
        try {
          val buf = new ListBuffer[A]
          batch.params foreach { ps =>
            setParams(prep, ps)
            if (prep.executeUpdate() > 0) autosMapper map { mapper =>
              val rs = prep.getGeneratedKeys
              try { if (rs.next()) buf += mapper(rs) }
              finally { rs.close() }
            }
          }
          buf.toList
        }
        finally { prep.close() }
      }
      finally { close(conn) }
    }
    catch {
      case ex: SQLException =>
        onError(batch, ex)
        if (throwOnInsert) throw ex else Nil
    }
  }

  //  returns number of rows affected
  def update(sql: SingleSQL): Int = {
    onSql(sql)
    try {
      val conn = getConnection
      try {
        val prep = conn prepareStatement sql.render
        try {
          setParams(prep, sql.params)
          prep.executeUpdate()
        }
        finally { prep.close() }
      }
      finally { close(conn) }
    }
    catch {
      case ex: SQLException =>
        onError(sql, ex)
        if (throwOnUpdate) throw ex else 0
    }
  }

  def delete(sql: SingleSQL) = update(sql)

  private def setParams(prep: PreparedStatement, ps: ParamsSeq) {
    ps.zipWithIndex foreach { case (param, index) => param set (prep, index + 1) }
  }
}