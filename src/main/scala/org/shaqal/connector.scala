package org.shaqal

import java.sql.Connection
import org.shaqal.sql._
import org.shaqal.sql.adapter._
import scala.util._

abstract class ConnectorBase {
  def getConnection(): Connection
  def close(conn: Connection): Unit
}

trait Counter extends ConnectorBase {
  var count: Long = _
  def resetCounter() = { count = 0 }
  abstract override def getConnection() = {
    count += 1
    super.getConnection()
  }
}

trait UseSingleConnection extends ConnectorBase {
  val conn = super.getConnection()
  abstract override def getConnection() = conn
  abstract override def close(conn: Connection) = {}
}

abstract class Connector[+D <: Database] extends ConnectorBase with DBOperations { conn =>

  implicit def connector: Connector[D] = this
  
  def throwOnQuery: Boolean
  def throwOnUpdate = true
  def throwOnInsert = true
  def throwOnDelete = true

  implicit val adapter: Adapter

  def onSql(sql: SQL)= {}
  def onError(t: Throwable) = { println(t) }
  def onError(sql: SQL, t: Throwable) = { println(sql.pp.render); println(t) }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T): Try[T]
  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T): Try[T]

  def onTransaction() = {}
  def onRollback() = {}
  def onCommit() = {}

  case class Debug(queries: Long, rows: Long, inserts: Long, batchInserts: Long, updates: Long, deletes: Long, millis: Long) {
    val qs = if (queries > 0) Some(s"$queries queries") else None
    val rs = if (rows > 0) Some(s"$rows rows") else None
    val is = if (inserts > 0) Some(s"$inserts inserts") else None
    val bis = if (batchInserts > 0) Some(s"$batchInserts batch inserts") else None
    val us = if (updates > 0) Some(s"$updates updates") else None
    val ds = if (deletes > 0) Some(s"$deletes deletes") else None
    val ms = Some(s"$millis ms")
    val results = Seq(qs, rs, is, bis, us, ds, ms).flatten mkString ", "
    override def toString = results
  }

  object Debug {

    private var q = 0
    private var r = 0
    private var i = 0
    private var bi = 0
    private var u = 0
    private var d = 0
    private var startTime = 0L

    def start() = {
      q = 0
      r = 0
      i = 0
      bi = 0
      u = 0
      d = 0
      startTime = System.currentTimeMillis
    }

    def end() = {
      val endTime = System.currentTimeMillis
      Debug(q, r, i, bi, u, d, endTime - startTime)
    }

    private[shaqal] def incQueries() = { q = q + 1 }
    private[shaqal] def incRows() = { r = r + 1 }
    private[shaqal] def incInserts() = { i = i + 1 }
    private[shaqal] def incBatchInserts() = { bi = bi + 1 }
    private[shaqal] def incUpdates() = { u = u + 1 }
    private[shaqal] def incDeletes() = { d = d + 1 }

    def numQueries = q
    def numRows = r
    def numInserts = i
    def numBatchInserts = bi
    def numUpdates = u
    def numDeletes = d
  }

}

abstract class DBC[D <: Database] extends Connector[D] with Transactions[D] {

  def close(conn: Connection) = { conn.close() }

  def checkConnection(): Either[Throwable, String] =
    try {
      close(getConnection())
      Right("OK")
    }
    catch {
      case ex: Exception =>
        onError(ex)
        Left(ex)
    }

  def throwOnQuery = false

  def createTXC(conn: Connection) = new TXC(this, conn)
}

class TXC[+D <: Database](dbc: DBC[D], conn: Connection) extends Connector[D] {

  val adapter = dbc.adapter

  def getConnection() = conn
  def close(conn: Connection) = {}

  def throwOnQuery = true

  override def onSql(sql: SQL) = { dbc onSql sql }
  override def onError(t: Throwable) = { dbc onError t }
  override def onError(sql: SQL, t: Throwable) = { dbc onError (sql, t) }
  override def onTransaction() = { dbc.onTransaction() }
  override def onCommit() = { dbc.onCommit() }
  override def onRollback() = { dbc.onRollback() }

  def commit() = {
    onCommit()
    conn.commit()
  }

  def rollback() = {
    onRollback()
    conn.rollback()
  }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T) = Success(tx(this))
  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T) = Success(tx(this))
}