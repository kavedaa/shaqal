package org.shaqal

import java.sql.Connection
import org.shaqal.sql._
import org.shaqal.sql.adapter._
import scala.util._

abstract class ConnectorBase {
  def getConnection: Connection
  def close(conn: Connection)  
}

trait Counter extends ConnectorBase { 
  var count: Long = _
  def resetCounter() { count = 0 }
  abstract override def getConnection = {
    count += 1
    super.getConnection
  }
}

trait UseSingleConnection extends ConnectorBase { 
  val conn = super.getConnection  
  abstract override def getConnection = conn
  abstract override def close(conn: Connection) {}
}

abstract class Connector[+D <: Database] extends ConnectorBase with DBOperations {
  
  def throwOnQuery: Boolean
  def throwOnUpdate = true
  def throwOnInsert = true
  def throwOnDelete = true

  implicit def adapter: Adapter
  
  def onSql(sql: SQL) {}
  def onError(t: Throwable) { println(t) }
  def onError(sql: SQL, t: Throwable) { println(sql.pp.render); println(t) }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T): Try[T]
  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T): Try[T]

  def onTransaction() {}
  def onRollback() {}
  def onCommit() {}
}

abstract class DBC[D <: Database] extends Connector[D] with Transactions[D] {

  def close(conn: Connection) { conn close () }

  def checkConnection(): Either[Throwable, String] =
    try {
      close(getConnection)
      Right("OK")
    } catch {
      case ex: Exception =>
        onError(ex)
        Left(ex)
    }

  def throwOnQuery = false
  
  def createTXC(conn: Connection) = new TXC(this, conn)
}

class TXC[+D <: Database](dbc: DBC[D], conn: Connection) extends Connector[D] {

  def adapter = dbc.adapter
  
  def getConnection = conn
  def close(conn: Connection) {}

  def throwOnQuery = true

  override def onSql(sql: SQL) { dbc onSql sql }
  override def onError(t: Throwable) { dbc onError t }
  override def onError(sql: SQL, t: Throwable) { dbc onError (sql, t) }
  override def onTransaction() { dbc onTransaction () }
  override def onCommit() { dbc onCommit () }
  override def onRollback() { dbc onRollback () }

  def commit() {
    onCommit()
    conn commit()
  }

  def rollback() {
    onRollback()
    conn rollback()
  }

  def transaction[T, E >: D <: Database](tx: TXC[E] => T) = Success(tx(this))
  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T) = Success(tx(this))
}