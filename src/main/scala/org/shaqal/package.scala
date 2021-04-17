package org

import scala.language.implicitConversions

import java.util.Date
import java.sql.Timestamp

package object shaqal {

  type -:[+D <: Database] = Connector[D]

  type DatabaseOf[+DD] = Database { type D <: DD }

  implicit def timestamp(d: Date): Timestamp = new Timestamp(d.getTime)
  implicit def timestamp(d: Option[Date]): Option[Timestamp] = d map (d => new Timestamp(d.getTime))

//  def tableExists[D <: InformationSchemaDB](table: TableLike, db: InformationSchemaDB)(implicit c: -:[D]) = {
//    db.InformationSchema.Tables where (_.table_name is table.tableName)
//  }
}