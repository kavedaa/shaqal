package org.shaqal

import org.shaqal._
import org.shaqal.sql._

sealed abstract class PKVal[+C]

final case class SomePKVal[C](value: C) extends PKVal[C]

case object * extends PKVal[Nothing]

object PKVal {
  implicit def pkVal[C](value: C) = new SomePKVal(value)
}

trait PKUtil {
  type ColOf[C] = Col { type T = C }  
}

trait ReadOnlyPK[C] extends PKUtil { this: Query =>

  val pk: ColOf[C]

  def at(value: C) = where(_ => pk is value)
  def existsAt(value: C)(implicit c: -:[D]) = at(value).exists
}

trait PK[C] extends ReadOnlyPK[C] { this: AccessorLike with Query =>
  
  override def at(value: C) = where(_ => pk is value)
  def updateAt(value: C) = updateWhere(_ => pk is value)
  def deleteAt(value: C)(implicit c: -:[D]) = deleteWhere(_ => pk is value)
}

trait ReadOnlyCompositePK extends PKUtil { this: Query =>

  protected val pk: Product

  protected def whereExprValues(values: Product) = {
    val pkExprs = (pk.productIterator zip values.productIterator).toSeq map {
      case (pk, value) =>
        pk.asInstanceOf[ColOf[Any]] is value
    }    
    AndExpr(pkExprs)
  }
  
  protected def whereExprSpecs(specs: Product) = {
    val pkExprs = (pk.productIterator zip specs.productIterator).toSeq collect {
      case (pk, SomePKVal(value)) =>
        pk.asInstanceOf[ColOf[Any]] is value
    }    
    if (pkExprs.isEmpty) True
    else AndExpr(pkExprs)
  }

  protected def atValues(values: Product) = where(_ => whereExprValues(values))
  protected def atSpecs(specs: Product) = where(_ => whereExprSpecs(specs))
  
  protected def existsAtValues(values: Product)(implicit c: -:[D]) = atValues(values).exists 
  protected def existsAtSpecs(specs: Product)(implicit c: -:[D]) = atSpecs(specs).exists
}

trait CompositePK extends ReadOnlyCompositePK { this: AccessorLike with Query =>
  
  protected override def atValues(values: Product) = where(_ => whereExprValues(values))
  protected override def atSpecs(specs: Product) = where(_ => whereExprSpecs(specs))
  
  protected def updateAtValues(values: Product) = updateWhere(_ => whereExprValues(values))
  protected def updateAtSpecs(specs: Product) = updateWhere(_ => whereExprSpecs(specs))
  
  protected def deleteAtValues(values: Product)(implicit c: -:[D]) = deleteWhere(_ => whereExprValues(values))
  protected def deleteAtSpecs(specs: Product)(implicit c: -:[D]) = deleteWhere(_ => whereExprSpecs(specs))
}

trait ReadOnlyPK2[C1, C2] extends ReadOnlyCompositePK { this: Query =>
  
  val pk: (ColOf[C1], ColOf[C2])
  
  def at(v1: PKVal[C1], v2: PKVal[C2]) = super.atSpecs((v1, v2))
  def existsAt(v1: PKVal[C1], v2: PKVal[C2])(implicit c: -:[D]) = super.existsAtSpecs((v1, v2))
}

trait PK2[C1, C2] extends ReadOnlyPK2[C1, C2] with CompositePK { this: AccessorLike with Query =>

  val pk: (ColOf[C1], ColOf[C2])
  
  override def at(v1: PKVal[C1], v2: PKVal[C2]) = super.atSpecs((v1, v2))
  def updateAt(v1: PKVal[C1], v2: PKVal[C2]) = super.updateAtSpecs((v1, v2))
  def deleteAt(v1: PKVal[C1], v2: PKVal[C2])(implicit c: -:[D]) = super.deleteAtSpecs((v1, v2))
}

trait ReadOnlyPK3[C1, C2, C3] extends ReadOnlyCompositePK { this: Query =>
  
  val pk: (ColOf[C1], ColOf[C2], ColOf[C3])
  
  def at(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3]) = super.atSpecs((v1, v2, v3))
  def existsAt(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3])(implicit c: -:[D]) = super.existsAtSpecs((v1, v2, v3))
}

trait PK3[C1, C2, C3] extends ReadOnlyPK3[C1, C2, C3] with CompositePK { this: AccessorLike with Query =>

  val pk: (ColOf[C1], ColOf[C2], ColOf[C3])
  
  override def at(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3]) = super.atSpecs((v1, v2, v3))
  def updateAt(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3]) = super.updateAtSpecs((v1, v2, v3))
  def deleteAt(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3])(implicit c: -:[D]) = super.deleteAtSpecs((v1, v2, v3))
}

trait ReadOnlyPK4[C1, C2, C3, C4] extends ReadOnlyCompositePK { this: Query =>
  
  val pk: (ColOf[C1], ColOf[C2], ColOf[C3], ColOf[C4])
  
  def at(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3], v4: PKVal[C4]) = super.atSpecs((v1, v2, v3, v4))
  def existsAt(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3], v4: PKVal[C4])(implicit c: -:[D]) = super.existsAtSpecs((v1, v2, v3, v4))
}

trait PK4[C1, C2, C3, C4] extends ReadOnlyPK4[C1, C2, C3, C4] with CompositePK { this: AccessorLike with Query =>

  val pk: (ColOf[C1], ColOf[C2], ColOf[C3], ColOf[C4])
  
  override def at(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3], v4: PKVal[C4]) = super.atSpecs((v1, v2, v3, v4))
  def updateAt(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3], v4: PKVal[C4]) = super.updateAtSpecs((v1, v2, v3, v4))
  def deleteAt(v1: PKVal[C1], v2: PKVal[C2], v3: PKVal[C3], v4: PKVal[C4])(implicit c: -:[D]) = super.deleteAtSpecs((v1, v2, v3, v4))
}
