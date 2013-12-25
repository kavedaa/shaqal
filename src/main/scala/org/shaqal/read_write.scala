package org.shaqal

import org.shaqal._
import org.shaqal.sql.ColumnParam
import java.sql.ResultSet

trait Readable {
  type T //	the raw datatype
  type F[X]
  type Q = F[T] //	the actual type that is read out, in practice T or Option[T]
  def cols: Seq[Col]
  def get(implicit rs: ResultSet): T
  def f[X](x: => X)(implicit rs: ResultSet): F[X]
  def read(implicit rs: ResultSet): Q = f(get)
}

trait Writable {
  type T
  type Q
  def set(v: T): ColumnParam
  def set(v: Option[T]): ColumnParam
  def :=(q: Q): ColumnParam
}

//	Experimental stuff below

trait NotNullReadable extends Readable { r =>

  type F[X] = X
  def f[X](x: => X)(implicit rs: ResultSet): F[X] = x

  def map[R](g: T => R): Readable = new NotNullReadable {
    type T = R
    def cols = r.cols
    def get(implicit rs: ResultSet) = g(r.get)
  }
}

object NotNullReadable {

  type NNROf[TT] = NotNullReadable { type T = TT }

  implicit class NNR2[T1, T2](r: (NNROf[T1], NNROf[T2])) {
    def map[R](g: (T1, T2) => R) = new NotNullReadable {
      type T = R
      def cols = r._1.cols ++ r._2.cols
      def get(implicit rs: ResultSet) = g(r._1.get, r._2.get)
    }
  }
}

trait NullableReadable extends Readable { r =>

  type F[X] = Option[X]
  def checkNull(implicit rs: ResultSet): Boolean
  def f[X](x: => X)(implicit rs: ResultSet): F[X] = if (checkNull) None else Some(x)

  def map[R](g: T => R): Readable = new NullableReadable {
    type T = R
    def cols = r.cols
    def checkNull(implicit rs: ResultSet) = r.checkNull
    def get(implicit rs: ResultSet) = ??? //	We can't safely run g on get as it may be null
    override def read(implicit rs: ResultSet) = r.read map g
  }

}

object NullableReadable {

  type NROf[TT] = NullableReadable { type T = TT }

  implicit class NR2[T1, T2](r: (NROf[T1], NROf[T2])) {
    def map[R](g: (T1, T2) => R) = new NullableReadable {
      type T = R
      def cols = r._1.cols ++ r._2.cols
      def checkNull(implicit rs: ResultSet) = r._1.checkNull || r._2.checkNull
      def get(implicit rs: ResultSet) = ???
      override def read(implicit rs: ResultSet) = (r._1.read, r._2.read) match {
        case (Some(t1), Some(t2)) => Some(g(t1, t2))
        case _ => None
      }
    }
  }
}

//	find a better name?
trait ReadWritable extends Readable with Writable

trait NotNullReadWritable extends ReadWritable {
  type F[X] = X
  def f[X](x: => X)(implicit rs: ResultSet): F[X] = x
  def :=(q: Q) = set(q)
}

trait NullableReadWritable extends ReadWritable {
  type F[X] = Option[X]
  def checkNull(implicit rs: ResultSet): Boolean
  def f[X](x: => X)(implicit rs: ResultSet): F[X] = if (checkNull) None else Some(x)
  def :=(q: Q) = set(q)
}

