package org.shaqal

import java.sql.SQLException
import scala.util._

trait Transactions[D <: Database] { this: DBC[D] =>

  def transaction[T, E >: D <: Database](tx: TXC[E] => T): Try[T] = {
    onTransaction()
    try {
      val conn = getConnection
      val autoCommit = conn.getAutoCommit
      try {
        conn setAutoCommit false
        Success(tx(createTXC(conn)))
      } 
      finally {
        conn setAutoCommit autoCommit
        close (conn)
      }
    } 
    catch {
      case t: Throwable =>
        onError(t)
        Failure(t)
    }
  }

  def autoTransaction[T, E >: D <: Database](tx: TXC[E] => T): Try[T] = {
    onTransaction()
    try {
      val conn = getConnection
      val autoCommit = conn.getAutoCommit
      try {
        conn setAutoCommit false
        val txc = createTXC(conn)
        try {
          val res = tx(txc)
          txc commit()
          Success(res)
        } 
        catch {
          case t: Throwable =>
            onError(t)
            txc rollback()
            Failure(t)
        }
      } 
      finally {
        conn setAutoCommit autoCommit
        close (conn)
      }
    } 
    catch {
      case t: Throwable =>
        onError(t)
        Failure(t)
    }
  }
}
