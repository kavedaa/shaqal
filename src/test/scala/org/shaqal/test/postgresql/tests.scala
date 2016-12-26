package org.shaqal.test.postgresql

import org.shaqal._
import org.shaqal.test.db.TestDB
import org.shaqal.sql.adapter.H2Adapter
import org.shaqal.sql.SQL
import org.shaqal.sql.adapter.PostgreSQLAdapter

class PostgreSQLDBC[D <: Database] extends UrlDBC[D](
  "jdbc:postgresql",
  "postgresql-test-db.properties",
  new org.postgresql.Driver) {

  implicit val adapter = PostgreSQLAdapter
}

trait PostgreSQL {
  implicit def dbc = new PostgreSQLDBC[TestDB] {
//    override def onSql(sql: SQL) = println(sql.pp.render)
  }
}

// class AdhocJoiningTest extends org.shaqal.test.AdhocJoiningTest with H2

class AggregateTest extends org.shaqal.test.AggregateTest with PostgreSQL

class ConstraintsTest extends org.shaqal.test.ConstraintsTest with PostgreSQL

class DataTypesTest extends org.shaqal.test.DataTypesTest with PostgreSQL

class DefinitionTest extends org.shaqal.test.DefinitionTest with PostgreSQL

class SelectTest extends org.shaqal.test.SelectTest with PostgreSQL

class TransactionsTest extends org.shaqal.test.TransactionsTest with PostgreSQL

class WhereTest extends org.shaqal.test.WhereTest with PostgreSQL

class CrudTest extends org.shaqal.test.CrudTest with PostgreSQL

class PKCrudTest extends org.shaqal.test.PKCrudTest with PostgreSQL

class PKMapperCrudTest extends org.shaqal.test.PKMapperCrudTest with PostgreSQL

class PK2MapperCrudTest extends org.shaqal.test.PK2MapperCrudTest with PostgreSQL

class AliasTest extends org.shaqal.test.AliasTest with PostgreSQL

class JoinOrderTest extends org.shaqal.test.JoinOrderTest with PostgreSQL

class SchemaTest extends org.shaqal.test.SchemaTest with PostgreSQL

class JoinTest extends org.shaqal.test.JoinTest with PostgreSQL

class Join2Test extends org.shaqal.test.Join2Test with PostgreSQL

class MapperJoinTest extends org.shaqal.test.MapperJoinTest with PostgreSQL

class ForUpdateTest extends org.shaqal.test.ForUpdateTest with PostgreSQL

//  postgres does not support "with lock" syntax
// class LockTest extends org.shaqal.test.LockTest with PostgreSQL