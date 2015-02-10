package org.shaqal

import org.shaqal._
import org.shaqal.sql._

trait Adhoc {

  type QueryOf[DD, RR] = Query {
    type D = DD
    type R = RR
    type QueryType = Query
  }

  class AdhocJoining[DD <: Database, R1, R2](q1: QueryOf[DD, R1], q2: QueryOf[DD, R2]) {

    def on[T](f: (R1, R2) => JoinExpr) = new Query { q =>

      type D = DD
      type R = (R1, R2)
      type QueryType = Query
      val r = (q1.r, q2.r)
      def fromItem = ??? // new JoinedItem(q1.r, Seq(new JoinElement(TableAlias(q2.r, ""), InnerJoin, f(q1.r, q2.r))))
      override def whereExpr = q1.whereExpr && q2.whereExpr

      def where(w: R => Expr): QueryOf[DD, (R1, R2)] = new Query {
        type QueryType = Query
        type D = q.D
        type R = q.R
        val r = q.r
        def fromItem = q.fromItem
        override def whereExpr = q.whereExpr && w(r)
        def where(w: R => Expr) = q.where(w)
      }

    }
  }

}

