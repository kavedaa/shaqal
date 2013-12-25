package org.shaqal.sql

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ExprTest extends FunSuite with ShouldMatchers {
  
  test("basic expressions") {
    
    False and False should equal(False)
    False and True should equal(False)
    True and False should equal(False)
    True and True should equal(True)
 
    OrExpr(List(True, True)) and False should equal(False)
    AndExpr(List(False, False)) or True should equal(True)
  }
  
}