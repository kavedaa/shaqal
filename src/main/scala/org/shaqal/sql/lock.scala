package org.shaqal.sql

case class Lock(render: String)

object Lock {

  final val UpdLock = Lock("updlock")
  final val RowLock = Lock("rowlock")

  def render(locks: Seq[Lock]) =
    s"(${locks map (_.render) mkString ", "})"
}