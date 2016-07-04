package org.shaqal

import org.shaqal.sql._
import java.sql.ResultSet

trait ReadOnlyPKMapperLike[A, C] extends ReadOnlyMapperLike[A] with ReadOnlyPK[C] with MapperQuery[A] {

  override type QueryType = MapperQuery[A]
  def apply(value: C)(implicit c: -:[D]) = at(value) option ()
}

trait ReadOnlyPKMapper[A, C] extends ReadOnlyPKMapperLike[A, C] with MapperQuery[A] {
  type R = this.type
  val r: R = this

  //  def cached = new CachedMapper[D, A, C](this)  
}

trait DualPKMapperLike[A, B, C] extends ReadOnlyPKMapperLike[A, C] with DualMapperLike[A, B] with PK[C] {

  val pkf: B => C

  def update(b: B)(implicit writer: W[B], c: -:[D]) = updateAt(pkf(b)) set b
  def delete(b: B)(implicit c: -:[D]) = deleteAt(pkf(b))

  def update(value: C, a: B)(implicit writer: W[B], c: -:[D]) = updateAt(value) set a

  def insertOrUpdate(b: B)(implicit writer: W[B], c: -:[D]): Either[Option[GG], Int] =
    if (existsAt(pkf(b))) Right(update(b)) else Left(insert(b))

  object PK { def apply(pk: ColOf[C], pkf: B => C) = (pk, pkf) }
}

trait DualPKMapper[A, B, C] extends DualPKMapperLike[A, B, C] with MapperQuery[A] {
  type R = this.type
  val r: R = this
}

trait PKMapper[A, C] extends DualPKMapper[A, A, C]

trait CompositePKLike[A] extends PKUtil { this: ReadOnlyMapperLike[A] with MapperQuery[A] =>
  val pk: Product
  //  val pkf: A => Product
  def expr(pkValues: Product) = pk.productIterator zip pkValues.productIterator map {
    case (pk, value) =>
      pk.asInstanceOf[ColOf[Any]] is value
  } reduceLeft { (left: Expr, right: Expr) => left && right }
  //  protected def apply(pkValues: Product)(implicit c: -:[D]) = where(_ => expr(pkValues)).option
  //  protected def exists(pkValues: Product)(implicit c: -:[D]) = apply(pkValues) isDefined
  //  def update(a: A)(implicit c: -:[D]) = updateWhere { _ => expr(pkMap(a)) } set a
  //  protected def updateAt(pkValues: Product)(implicit c: -:[D]) = updateWhere { _ => expr(pkValues) }
  //  def insertOrUpdate(a: A)(implicit c: -:[D]) = if (exists(pkMap(a))) Right(update(a)) else Left(insert(a))
  //  def delete(a: A)(implicit c: -:[D]) = deleteWhere { _ => expr(pkMap(a)) }
}

trait ReadOnlyPK2MapperLike[A, C1, C2] extends CompositePKLike[A] with ReadOnlyMapperLike[A] with ReadOnlyPK2[C1, C2] with MapperQuery[A] {

  def apply(v: (C1, C2))(implicit c: -:[D]) = at(v._1, v._2) option ()
  def apply(v1: C1, v2: C2)(implicit c: -:[D]) = at(v1, v2) option ()
}

trait ReadOnlyPK2Mapper[A, C1, C2] extends ReadOnlyPK2MapperLike[A, C1, C2] { pkm =>

  type R = this.type
  val r: R = this

  //  def apply(pkValues: (C1, C2))(implicit c: -:[D]) = super.apply(pkValues)
  //  def exists(pkValues: (C1, C2))(implicit c: -:[D]) = super.exists(pkValues)
  //  def updateAt(pkValues: (C1, C2))(implicit c: -:[D]) = super.updateAt(pkValues)

  //  def cached = new Cached2Mapper[D, A, C1, C2](this)
}

trait DualPK2MapperLike[A, B, C1, C2] extends ReadOnlyPK2MapperLike[A, C1, C2] with DualMapperLike[A, B] with PK2[C1, C2] {

  val pkf: B => (C1, C2)

  def update(b: B)(implicit writer: W[B], c: -:[D]) = updateAtValues(pkf(b)) set b
  def delete(b: B)(implicit c: -:[D]) = deleteAtValues(pkf(b))

  def update(values: (C1, C2), a: B)(implicit writer: W[B], c: -:[D]) = updateAtValues(values) set a

  def insertOrUpdate(b: B)(implicit writer: W[B], c: -:[D]): Either[Option[GG], Int] =
    if (existsAtValues(pkf(b))) Right(update(b)) else Left(insert(b))

  object PK { def apply(pk: (ColOf[C1], ColOf[C2]), pkf: B => (C1, C2)) = (pk, pkf) }
}

trait DualPK2Mapper[A, B, C1, C2] extends DualPK2MapperLike[A, B, C1, C2] with MapperQuery[A] {
  type R = this.type
  val r: R = this
}

trait PK2Mapper[A, C1, C2] extends DualPK2Mapper[A, A, C1, C2]

trait ReadOnlyPK3MapperLike[A, C1, C2, C3] extends CompositePKLike[A] with ReadOnlyMapperLike[A] with ReadOnlyPK3[C1, C2, C3] with MapperQuery[A] {

  def apply(v: (C1, C2, C3))(implicit c: -:[D]) = at(v._1, v._2, v._3) option ()
  def apply(v1: C1, v2: C2, v3: C3)(implicit c: -:[D]) = at(v1, v2, v3) option ()
}

trait ReadOnlyPK3Mapper[A, C1, C2, C3] extends ReadOnlyPK3MapperLike[A, C1, C2, C3] {

  type R = this.type
  val r: R = this
}

trait DualPK3MapperLike[A, B, C1, C2, C3] extends ReadOnlyPK3MapperLike[A, C1, C2, C3] with DualMapperLike[A, B] with PK3[C1, C2, C3] {

  val pkf: A => (C1, C2, C3)

  def update(a: A)(implicit writer: W[A], c: -:[D]) = updateAtValues(pkf(a)) set a
  def delete(a: A)(implicit c: -:[D]) = deleteAtValues(pkf(a))

  def update(values: (C1, C2, C3), a: A)(implicit writer: W[A], c: -:[D]) = updateAtValues(values) set a

  def insertOrUpdate(a: A)(implicit writer: W[A], c: -:[D]): Either[Option[GG], Int] =
    if (existsAtValues(pkf(a))) Right(update(a)) else Left(insert(a))

  object PK { def apply(pk: (ColOf[C1], ColOf[C2], ColOf[C3]), pkf: B => (C1, C2, C3)) = (pk, pkf) }
}

trait DualPK3Mapper[A, B, C1, C2, C3] extends DualPK3MapperLike[A, B, C1, C2, C3] with MapperQuery[A] {
  type R = this.type
  val r: R = this
}

trait PK3Mapper[A, C1, C2, C3] extends DualPK3Mapper[A, A, C1, C2, C3]

trait ReadOnlyPK4MapperLike[A, C1, C2, C3, C4] extends CompositePKLike[A] with ReadOnlyMapperLike[A] with ReadOnlyPK4[C1, C2, C3, C4] with MapperQuery[A] {

  def apply(v: (C1, C2, C3, C4))(implicit c: -:[D]) = at(v._1, v._2, v._3, v._4) option ()
  def apply(v1: C1, v2: C2, v3: C3, v4: C4)(implicit c: -:[D]) = at(v1, v2, v3, v4) option ()
}

trait ReadOnlyPK4Mapper[A, C1, C2, C3, C4] extends ReadOnlyPK4MapperLike[A, C1, C2, C3, C4] {

  type R = this.type
  val r: R = this
}

trait DualPK4MapperLike[A, B, C1, C2, C3, C4] extends ReadOnlyPK4MapperLike[A, C1, C2, C3, C4] with DualMapperLike[A, B] with PK4[C1, C2, C3, C4] {

  val pkf: A => (C1, C2, C3, C4)

  def update(a: A)(implicit writer: W[A], c: -:[D]) = updateAtValues(pkf(a)) set a
  def delete(a: A)(implicit c: -:[D]) = deleteAtValues(pkf(a))

  def update(values: (C1, C2, C3, C4), a: A)(implicit writer: W[A], c: -:[D]) = updateAtValues(values) set a

  def insertOrUpdate(a: A)(implicit writer: W[A], c: -:[D]): Either[Option[GG], Int] =
    if (existsAtValues(pkf(a))) Right(update(a)) else Left(insert(a))

  object PK { def apply(pk: (ColOf[C1], ColOf[C2], ColOf[C3], ColOf[C4]), pkf: B => (C1, C2, C3, C4)) = (pk, pkf) }
}

trait DualPK4Mapper[A, B, C1, C2, C3, C4] extends DualPK4MapperLike[A, B, C1, C2, C3, C4] with MapperQuery[A] {
  type R = this.type
  val r: R = this
}

trait PK4Mapper[A, C1, C2, C3, C4] extends DualPK4Mapper[A, A, C1, C2, C3, C4]


trait ReadOnlyPK7MapperLike[A, C1, C2, C3, C4, C5, C6, C7] extends CompositePKLike[A] with ReadOnlyMapperLike[A] with ReadOnlyPK7[C1, C2, C3, C4, C5, C6, C7] with MapperQuery[A] {

  def apply(v: (C1, C2, C3, C4, C5, C6, C7))(implicit c: -:[D]) = at(v._1, v._2, v._3, v._4, v._5, v._6, v._7) option ()
  def apply(v1: C1, v2: C2, v3: C3, v4: C4, v5: C5, v6: C6, v7: C7)(implicit c: -:[D]) = at(v1, v2, v3, v4, v5, v6, v7) option ()
}

trait ReadOnlyPK7Mapper[A, C1, C2, C3, C4, C5, C6, C7] extends ReadOnlyPK7MapperLike[A, C1, C2, C3, C4, C5, C6, C7] {

  type R = this.type
  val r: R = this
}

trait DualPK7MapperLike[A, B, C1, C2, C3, C4, C5, C6, C7] extends ReadOnlyPK7MapperLike[A, C1, C2, C3, C4, C5, C6, C7] with DualMapperLike[A, B] with PK7[C1, C2, C3, C4, C5, C6, C7] {

  val pkf: A => (C1, C2, C3, C4, C5, C6, C7)

  def update(a: A)(implicit writer: W[A], c: -:[D]) = updateAtValues(pkf(a)) set a
  def delete(a: A)(implicit c: -:[D]) = deleteAtValues(pkf(a))

  def update(values: (C1, C2, C3, C4, C5, C6, C7), a: A)(implicit writer: W[A], c: -:[D]) = updateAtValues(values) set a

  def insertOrUpdate(a: A)(implicit writer: W[A], c: -:[D]): Either[Option[GG], Int] =
    if (existsAtValues(pkf(a))) Right(update(a)) else Left(insert(a))

  object PK { def apply(pk: (ColOf[C1], ColOf[C2], ColOf[C3], ColOf[C4], ColOf[C5], ColOf[C6], ColOf[C7]), pkf: B => (C1, C2, C3, C4, C5, C6, C7)) = (pk, pkf) }
}

trait DualPK7Mapper[A, B, C1, C2, C3, C4, C5, C6, C7] extends DualPK7MapperLike[A, B, C1, C2, C3, C4, C5, C6, C7] with MapperQuery[A] {
  type R = this.type
  val r: R = this
}

trait PK7Mapper[A, C1, C2, C3, C4, C5, C6, C7] extends DualPK7Mapper[A, A, C1, C2, C3, C4, C5, C6, C7]

