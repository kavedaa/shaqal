package org.shaqal.test.feature

import org.shaqal._
import org.shaqal.test.db.TestDB
import org.shaqal.sql.adapter.H2Adapter

class H2DBC[D <: Database](name: String) extends UrlDBC[D](
  s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1",
  new org.h2.Driver,
  "sa",
  "") with UseSingleConnection {

  def adapter = H2Adapter
  
  override def onTransaction() { println("transaction") }
}

trait H2 {
  implicit def dbc = new H2DBC[TestDB]("test")   
}

class AdhocJoiningTest extends org.shaqal.test.AdhocJoiningTest with H2

class AggregateTest extends org.shaqal.test.AggregateTest with H2

class ConstraintsTest extends org.shaqal.test.ConstraintsTest with H2

class DataTypesTest extends org.shaqal.test.DataTypesTest with H2

//	some features tested here are not supported on H2
// class DefinitionTest extends org.shaqal.test.DefinitionTest with H2

class SelectTest extends org.shaqal.test.SelectTest with H2

class TransactionsTest extends org.shaqal.test.TransactionsTest with H2

class WhereTest extends org.shaqal.test.WhereTest with H2

class CrudTest extends org.shaqal.test.CrudTest with H2

class PKCrudTest extends org.shaqal.test.PKCrudTest with H2

class PKMapperCrudTest extends org.shaqal.test.PKMapperCrudTest with H2

class PK2MapperCrudTest extends org.shaqal.test.PK2MapperCrudTest with H2