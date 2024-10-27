package org.shaqal.sql

import org.scalatest.funsuite._
import org.scalatest.matchers.should._

class ExprTest extends AnyFunSuite with Matchers {
  
  test("basic expressions") {
    
    False and False should equal(False)
    False and True should equal(False)
    True and False should equal(False)
    True and True should equal(True)
 
    OrExpr(List(True, True)) and False should equal(False)
    AndExpr(List(False, False)) or True should equal(True)
  }
  
}