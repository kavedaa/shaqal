package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet
import scala.collection.generic.CanBuildFrom

trait Selecting { this: Query =>

  def select[S, A](f: R => S)(implicit selecter: Selecter[S], selectReader: SelectReader[S, A]) = {
    val s = f(r)
    new SelectHolder[S, A](r, s, selectReader, selectSql(selecter(s) flatMap (_.cols)))
  }

  class SelectHolder[S, A](r: R, s: S, selectReader: SelectReader[S, A], sql: SelectSQL) {

    def query[U](f: A => U)(implicit c: -:[D]) =
      c query(sql, selectReader(s), f)
    
    def into[Coll[_]]()(implicit cbf: CanBuildFrom[Nothing, A, Coll[A]], c: -:[D]) =
      c queryColl (sql, selectReader(s), cbf())
    
    def list()(implicit c: -:[D]) = into[List]
    
    def option()(implicit c: -:[D]) = list.headOption
    
    def apply[Coll[_]]()(implicit cbf: CanBuildFrom[Nothing, A, Coll[A]], c: -:[D]): Coll[A] = into[Coll]
  }

}

abstract class Selecter[-S] extends (S => Seq[Readable])

object Selecter {

  implicit val singleSelecter = new Selecter[Readable] {
    def apply(readable: Readable) = Seq(readable)
  }

  implicit val seqSelecter = new Selecter[Seq[Readable]] {
    def apply(seq: Seq[Readable]) = seq
  }

  implicit val tuple2Selecter = new Selecter[(Readable, Readable)] {
    def apply(t: (Readable, Readable)) = t.productIterator.toSeq.asInstanceOf[Seq[Readable]]
  }

  implicit val tuple3Selecter = new Selecter[(Readable, Readable, Readable)] {
    def apply(t: (Readable, Readable, Readable)) = t.productIterator.toSeq.asInstanceOf[Seq[Readable]]
  }
  
//  implicit def tuple2Selecter[Q1, Q2] = new Selecter[(ReadableOf[Q1], ReadableOf[Q2])] {
//    def apply(t: (ReadableOf[Q1], ReadableOf[Q2])) = t.productIterator.toSeq.asInstanceOf[Seq[Readable]]
//  }
}

abstract class SelectReader[-S, A] extends (S => Reader[A])

object SelectReader {

  implicit def singleReader[R <: Readable] = new SelectReader[R, R#Q] {
    def apply(readable: R): Reader[R#Q] = new Reader[R#Q] {
      def apply(rs: ResultSet) = readable read rs
    }
  }

  implicit def tuple2Reader[R1 <: Readable, R2 <: Readable] = new SelectReader[(R1, R2), (R1#Q, R2#Q)] {
    def apply(t: (R1, R2)): Reader[(R1#Q, R2#Q)] = new Reader[(R1#Q, R2#Q)] {
      def apply(rs: ResultSet) = (t._1 read rs, t._2 read rs)
    }
  }
  
  implicit def tuple3Reader[R1 <: Readable, R2 <: Readable, R3 <: Readable] = new SelectReader[(R1, R2, R3), (R1#Q, R2#Q, R3#Q)] {
    def apply(t: (R1, R2, R3)): Reader[(R1#Q, R2#Q, R3#Q)] = new Reader[(R1#Q, R2#Q, R3#Q)] {
      def apply(rs: ResultSet) = (t._1 read rs, t._2 read rs, t._3 read rs)
    }
  }
  
}

