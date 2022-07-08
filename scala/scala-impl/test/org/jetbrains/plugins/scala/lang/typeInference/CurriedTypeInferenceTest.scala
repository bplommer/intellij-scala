package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

class CurriedTypeInferenceTest extends ScalaLightCodeInsightFixtureTestAdapter {
  def testSCL7332(): Unit = checkTextHasNoErrors(
    """
      |class Foo[A, B](a: A, b: B)(f: B => A)
      |
      |val foo1 = new Foo(1, identity[Int] _)({ case f => f(2) })
      |val foo2 = new Foo(1, identity[Int] _)(f => f(2))
    """.stripMargin)
}
