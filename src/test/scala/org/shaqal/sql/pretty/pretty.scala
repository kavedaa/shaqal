package org.shaqal.sql.pretty

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

class PrettyTest extends FunSuite with ShouldMatchers {

  test("simple pretty rendering and unprettify") {
    
    val pretty = ElementList("test", Indent("foo"), CommaLines(List("bar", "zip")).parens)
    
    Pretty.unprettify(pretty.render) should equal("test foo (bar, zip)")
  }
}