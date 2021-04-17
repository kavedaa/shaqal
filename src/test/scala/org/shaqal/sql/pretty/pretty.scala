package org.shaqal.sql.pretty

import org.scalatest.funsuite._
import org.scalatest.matchers.should._

class PrettyTest extends AnyFunSuite with Matchers {

  test("simple pretty rendering and unprettify") {
    
    val pretty = ElementList("test", Indent("foo"), CommaLines(List("bar", "zip")).parens)
    
    Pretty.unprettify(pretty.render).shouldEqual("test foo (bar, zip)")
  }
}