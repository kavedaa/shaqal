package org.shaqal.test.integration.mssql

import org.shaqal._
import org.shaqal.sql.adapter.MSSQLAdapter
import net.sourceforge.jtds.jdbcx.JtdsDataSource
import org.shaqal.sql.SQL
import org.shaqal.test.db.TestDB

object JtdsFactory extends DataSourceFactory {
  def getDataSource(server: String, database: String, port: Int) = {
    val ds = new JtdsDataSource
    ds setServerName (server)
    ds setDatabaseName (database)
    ds setPortNumber (port)
    ds
  }
}

class MSSQLDBC[D <: Database]
  extends DataSourceDBC[D]("mssql-test-db.properties", JtdsFactory)
  with UseSingleConnection {
  
  override implicit val adapter = MSSQLAdapter
  
//  override def onSql(sql: SQL) { println(sql.pp.render)}
}
   
trait MSSQL {
  implicit def dbc = new MSSQLDBC[TestDB]   {
//    override def onSql(sql: SQL) = println(sql.pp.render)
  }
}

// class AdhocJoiningTest extends org.shaqal.test.AdhocJoiningTest with MSSQL

class AggregateTest extends org.shaqal.test.AggregateTest with MSSQL

class ConstraintsTest extends org.shaqal.test.ConstraintsTest with MSSQL

class DataTypesTest extends org.shaqal.test.DataTypesTest with MSSQL

class DefinitionTest extends org.shaqal.test.DefinitionTest with MSSQL

class InheritDBTest extends org.shaqal.test.InheritDBTest {
  implicit def dbc = new MSSQLDBC[SubDB]  
}

class SelectTest extends org.shaqal.test.SelectTest with MSSQL

class TransactionsTest extends org.shaqal.test.TransactionsTest with MSSQL

class WhereTest extends org.shaqal.test.WhereTest with MSSQL

class CrudTest extends org.shaqal.test.CrudTest with MSSQL

class PKCrudTest extends org.shaqal.test.PKCrudTest with MSSQL

class AliasTest extends org.shaqal.test.AliasTest with MSSQL

class JoinOrderTest extends org.shaqal.test.JoinOrderTest with MSSQL

class SchemaTest extends org.shaqal.test.SchemaTest with MSSQL

class JoinTest extends org.shaqal.test.JoinTest with MSSQL

class Join2Test extends org.shaqal.test.Join2Test with MSSQL
