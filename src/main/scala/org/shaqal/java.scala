package org.shaqal.javautil

import org.shaqal._

import java.sql.ResultSet

//	Misc. wrappers for Java

abstract class ResultMapper[T] extends (ResultSet => T)

abstract class TransactionWrapper[D <: Database, T] extends (TXC[D] => T)
  
