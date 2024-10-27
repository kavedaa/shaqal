package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet

import scala.collection.Factory
import scala.util.Try

trait Selecting { this: Query =>

  def select[S, A](f: R => S)(implicit selecter: Selecter[S], selectReader: SelectReader[S, A]) = {
    val s = f(r)
    new SelectHolder[S, A](r, s, selectReader, selectSql(selecter(s) flatMap (_.cols)))
  }

  def selectWithLock[S, A](locks: Lock*)(f: R => S)(implicit selecter: Selecter[S], selectReader: SelectReader[S, A]) = {
    val s = f(r)
    new SelectHolder[S, A](r, s, selectReader, selectSql(selecter(s) flatMap (_.cols), false, locks))
  }

  def selectForUpdate[S, A](f: R => S)(implicit selecter: Selecter[S], selectReader: SelectReader[S, A]) = {
    val s = f(r)
    new SelectHolder[S, A](r, s, selectReader, selectSql(selecter(s) flatMap (_.cols), true))
  }

  class SelectHolder[S, A](r: R, s: S, selectReader: SelectReader[S, A], sql: SelectSQL) {

    def query[U](f: A => U)(implicit c: -:[D]) =
      c query (sql, selectReader(s), f)

    def into[Coll[_]](factory: Factory[A, Coll[A]])(implicit c: -:[D]) =
      c queryColl (sql, selectReader(s), factory.newBuilder)

    def list()(implicit c: -:[D]) = into(List)
    
//    def set()(implicit c: -:[D]) = into[Set]

    def option()(implicit c: -:[D]) = list().headOption

    def get()(implicit c: -:[D]) = list().head

//    def apply[Coll[_]]()(implicit cbf: BuildFrom[Nothing, A, Coll[A]], c: -:[D]): Coll[A] = into[Coll]
  }

}

abstract class Selecter[-S] extends (S => Seq[Readable])

object Selecter {

  implicit val singleSelecter: Selecter[Readable] = new Selecter[Readable] {
    def apply(readable: Readable) = Seq(readable)
  }

  implicit val seqSelecter: Selecter[Seq[Readable]] = new Selecter[Seq[Readable]] {
    def apply(seq: Seq[Readable]) = seq
  }

  implicit val tuple2Selecter: Selecter[(Readable, Readable)] = new Selecter[(Readable, Readable)] {
    def apply(t: (Readable, Readable)) = t.productIterator.toSeq.asInstanceOf[Seq[Readable]]
  }

  implicit val tuple3Selecter: Selecter[(Readable, Readable, Readable)] = new Selecter[(Readable, Readable, Readable)] {
    def apply(t: (Readable, Readable, Readable)) = t.productIterator.toSeq.asInstanceOf[Seq[Readable]]
  }

  //  implicit def tuple2Selecter[Q1, Q2] = new Selecter[(ReadableOf[Q1], ReadableOf[Q2])] {
  //    def apply(t: (ReadableOf[Q1], ReadableOf[Q2])) = t.productIterator.toSeq.asInstanceOf[Seq[Readable]]
  //  }
}

abstract class SelectReader[-S, A] extends (S => Reader[A])

object SelectReader {

  type ReadableOf[QQ] = Readable { type Q = QQ }

  implicit def singleReader[Q, R <: ReadableOf[Q]]: SelectReader[R, Q] = new SelectReader {
    def apply(readable: R): Reader[Q] = new Reader {
      def apply(rs: ResultSet) = readable.read(rs)
    }
  }

  implicit def tuple2Reader[Q1, Q2, R1 <: ReadableOf[Q1], R2 <: ReadableOf[Q2]]: SelectReader[(R1, R2), (Q1, Q2)] = new SelectReader {
    def apply(t: (R1, R2)): Reader[(Q1, Q2)] = new Reader {
      def apply(rs: ResultSet) = (t._1 read rs, t._2 read rs)
    }
  }

  // implicit def tuple3Reader[R1 <: Readable, R2 <: Readable, R3 <: Readable]: SelectReader[(R1, R2, R3), (R1#Q, R2#Q, R3#Q)] = new SelectReader[(R1, R2, R3), (R1#Q, R2#Q, R3#Q)] {
  //   def apply(t: (R1, R2, R3)): Reader[(R1#Q, R2#Q, R3#Q)] = new Reader[(R1#Q, R2#Q, R3#Q)] {
  //     def apply(rs: ResultSet) = (t._1 read rs, t._2 read rs, t._3 read rs)
  //   }
  // }

}

