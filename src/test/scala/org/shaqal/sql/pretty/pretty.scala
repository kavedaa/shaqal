package org.shaqal.sql.pretty

import org.scalatest._

class PrettyTest extends FunSuite with Matchers {

  test("simple pretty rendering and unprettify") {
    
    val pretty = ElementList("test", Indent("foo"), CommaLines(List("bar", "zip")).parens)
    
    Pretty.unprettify(pretty.render) should equal("test foo (bar, zip)")
  }
}