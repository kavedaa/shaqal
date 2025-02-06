package org.shaqal.test.h2

import org.shaqal._
import org.shaqal.test.db.TestDB
import org.shaqal.sql.adapter.H2Adapter
import org.shaqal.sql.SQL

class H2DBC[D <: Database](name: String) extends UrlDBC[D](
  s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1",
  new org.h2.Driver,
  "sa",
  "") with UseSingleConnection {

  implicit val adapter = H2Adapter

//  override def onTransaction() { println("transaction") }
}

trait H2 {
  implicit def dbc: DBC[TestDB] = new H2DBC[TestDB]("test") {
        override def onSql(sql: SQL) = println(sql.pp.render)
  }
}

// class AdhocJoiningTest extends org.shaqal.test.AdhocJoiningTest with H2

class AggregateTest extends org.shaqal.test.AggregateTest with H2

class ConstraintsTest extends org.shaqal.test.ConstraintsTest with H2

class DataTypesTest extends org.shaqal.test.DataTypesTest with H2

class DefinitionTest extends org.shaqal.test.DefinitionTest with H2

class SelectTest extends org.shaqal.test.SelectTest with H2

class TransactionsTest extends org.shaqal.test.TransactionsTest with H2

class WhereTest extends org.shaqal.test.WhereTest with H2

class CrudTest extends org.shaqal.test.CrudTest with H2

class PKCrudTest extends org.shaqal.test.PKCrudTest with H2

class PKMapperCrudTest extends org.shaqal.test.PKMapperCrudTest with H2

class PK2MapperCrudTest extends org.shaqal.test.PK2MapperCrudTest with H2

class AliasTest extends org.shaqal.test.AliasTest with H2

class JoinOrderTest extends org.shaqal.test.JoinOrderTest with H2

class SchemaTest extends org.shaqal.test.SchemaTest with H2

class JoinTest extends org.shaqal.test.JoinTest with H2

class Join2Test extends org.shaqal.test.Join2Test with H2

class MapperJoinTest extends org.shaqal.test.MapperJoinTest with H2

class ForUpdateTest extends org.shaqal.test.ForUpdateTest with H2

//  H2 does not support locks (?)
// class LockTest extends org.shaqal.test.LockTest with H2