package org.shaqal

import java.sql.Driver
import java.sql.DriverManager
import javax.naming.InitialContext
import javax.sql.DataSource
import java.util.Properties
import java.io.InputStream
import java.io.FileInputStream
import java.io.File

abstract class UrlDBC[D <: Database](
//  val description: String,
  url: String,
  driver: Driver,
  username: String,
  password: String)
extends DBC[D] {

  DriverManager registerDriver driver

  def getConnection() = DriverManager getConnection (url, username, password)
}

abstract class NamedDataSourceDBC[D <: Database](
//  val description: String,
  name: String)
extends DBC[D] {

  val ds = (new InitialContext).lookup(name).asInstanceOf[DataSource]

  def getConnection() = ds getConnection ()
}

abstract class DataSourceDBC[D <: Database](
//  val description: String,
  server: String,
  database: String,
  port: Int,
  username: String,
  password: String,
  dsFactory: DataSourceFactory)
extends DBC[D] {

  def this(properties: Properties, dsFactory: DataSourceFactory) =
    this(
//      description,
      properties getProperty "server",
      properties getProperty "database",
      properties getProperty "port" toInt,
      properties getProperty "username",
      properties getProperty "password",
      dsFactory)

  def this(propertiesStream: InputStream, dsFactory: DataSourceFactory) =
    this(
//      description,
      {
        val properties = new Properties
        properties load propertiesStream
        properties
      },
      dsFactory)

  def this(propertiesFile: File, dsFactory: DataSourceFactory) =
    this(
//      description,
      new FileInputStream(propertiesFile),
      dsFactory)

  def this(propertiesFileName: String, dsFactory: DataSourceFactory) =
    this(
//      description,
      new File(propertiesFileName),
      dsFactory)

  def getConnection = dsFactory getDataSource (server, database, port) getConnection (username, password)
}

abstract class DataSourceFactory {
  def getDataSource(server: String, database: String, port: Int): DataSource
}