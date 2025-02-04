package org.jetbrains.plugins.scala.lang.dfa.invocationInfo.tests

import org.jetbrains.plugins.scala.lang.dfa.invocationInfo.InvocationInfoTestBase
import org.jetbrains.plugins.scala.lang.dfa.invocationInfo.arguments.Argument.{PassByName, PassByValue}

class RegularMethodCallInfoTest extends InvocationInfoTestBase {

  def testSimpleMethodCalls(): Unit = {
    val invocationInfo = generateInvocationInfoFor {
      s"""
         |class SomeClass {
         |  def simpleFun(firstArg: Int, secondArg: Boolean, thirdArg: String, fourthArg: Int): Int = {
         |    firstArg + fourthArg
         |  }
         |
         |  def main(): Int = {
         |    ${markerStart}simpleFun(3 + 8, 5 > 9, "Hello", 9 * 4 - 2)${markerEnd}
         |  }
         |}
         |""".stripMargin
    }

    val expectedArgCount = 1 + 4 // implicit "this" argument
    val expectedProperArgsInText = List("3 + 8", "5 > 9", "\"Hello\"", "9 * 4 - 2")
    val expectedMappedParamNames = List("firstArg", "secondArg", "thirdArg", "fourthArg")
    val expectedPassingMechanisms = (1 to expectedArgCount).map(_ => PassByValue).toList
    val expectedParamToArgMapping = (0 until expectedArgCount - 1).toList

    verifyInvokedElement(invocationInfo, "SomeClass#simpleFun")
    verifyArgumentsWithSingleArgList(invocationInfo, expectedArgCount, expectedProperArgsInText,
      expectedMappedParamNames, expectedPassingMechanisms, expectedParamToArgMapping)
  }

  def testByNameArguments(): Unit = {
    val invocationInfo = generateInvocationInfoFor {
      s"""
         |class AnotherClass {
         |  def funWithByNames(arg0: Int, arg1: => Boolean, arg2: String, arg3: => Int): Int = {
         |    firstArg + fourthArg
         |  }
         |
         |  def main(): Int = {
         |    val x = 3
         |    ${markerStart}funWithByNames(328944 * 22, 5 >= 3 && false, "Hello", -3324 + x)${markerEnd}
         |  }
         |}
         |""".stripMargin
    }

    val expectedArgCount = 1 + 4
    val expectedProperArgsInText = List("328944 * 22", "5 >= 3 && false", "\"Hello\"", "-3324 + x")
    val expectedMappedParamNames = List("arg0", "arg1", "arg2", "arg3")
    val expectedPassingMechanisms = List(PassByValue, PassByValue, PassByName, PassByValue, PassByName)
    val expectedParamToArgMapping = (0 until expectedArgCount - 1).toList

    verifyInvokedElement(invocationInfo, "AnotherClass#funWithByNames")
    verifyArgumentsWithSingleArgList(invocationInfo, expectedArgCount, expectedProperArgsInText,
      expectedMappedParamNames, expectedPassingMechanisms, expectedParamToArgMapping)
  }

  def testMethodCallsOnInstance(): Unit = {
    val invocationInfo = generateInvocationInfoFor {
      s"""
         |object TestObject {
         |  class Something(z: Double) {
         |    def compareWith(x: Double, y: Double): Boolean = z < x && z < y
         |  }
         |
         |  def main(): Int = {
         |    val newSomething = new Something(12.34)
         |    ${markerStart}newSomething.compareWith(19.52 * 2.5, -11.0034 * (-1))${markerEnd}
         |  }
         |}
         |""".stripMargin
    }

    val expectedArgCount = 1 + 2
    val expectedProperArgsInText = List("19.52 * 2.5", "-11.0034 * (-1)")
    val expectedMappedParamNames = List("x", "y")
    val expectedPassingMechanisms = (1 to expectedArgCount).map(_ => PassByValue).toList
    val expectedParamToArgMapping = (0 until expectedArgCount - 1).toList

    verifyInvokedElement(invocationInfo, "Something#compareWith")
    verifyArgumentsWithSingleArgList(invocationInfo, expectedArgCount, expectedProperArgsInText,
      expectedMappedParamNames, expectedPassingMechanisms, expectedParamToArgMapping)
    verifyThisExpression(invocationInfo, "newSomething")
  }

  def testJavaStaticMethods(): Unit = {
    val invocationInfo = generateInvocationInfoFor {
      s"""
         |import java.time.LocalDate
         |
         |object TestObject {
         |
         |  def main(): Int = {
         |  	val date = ${markerStart}LocalDate.of(2012, 11, 23)${markerEnd}
         |  	date.getYear
         |  }
         |}
         |
         |""".stripMargin
    }

    val expectedArgCount = 1 + 3
    val expectedProperArgsInText = List("2012", "11", "23")
    val expectedMappedParamNames = List("year", "month", "dayOfMonth")
    val expectedPassingMechanisms = (1 to expectedArgCount).map(_ => PassByValue).toList
    val expectedParamToArgMapping = (0 until expectedArgCount - 1).toList

    verifyInvokedElement(invocationInfo, "LocalDate#of")
    verifyArgumentsWithSingleArgList(invocationInfo, expectedArgCount, expectedProperArgsInText,
      expectedMappedParamNames, expectedPassingMechanisms, expectedParamToArgMapping)
    verifyThisExpression(invocationInfo, "LocalDate")
  }

  def testLocalFunctionsWithParams(): Unit = {
    val sugaredSyntax = "local(5, 9)"
    val desugaredSyntax = "local.apply(5, 9)"

    val code = (invocationSyntax: String) =>
      s"""
         |object TestObject {
         |
         |  def main(): Int = {
         |  	val local = (x: Int, y: Int) => x + y
         |    ${markerStart}${invocationSyntax}${markerEnd}
         |  }
         |}
         |
         |""".stripMargin

    for (invocationSyntax <- List(sugaredSyntax, desugaredSyntax)) {
      val invocationInfo = generateInvocationInfoFor(code(invocationSyntax))

      val expectedArgCount = 1 + 2
      val expectedProperArgsInText = List("5", "9")
      val expectedMappedParamNames = List("v1", "v2")
      val expectedPassingMechanisms = (1 to expectedArgCount).map(_ => PassByValue).toList
      val expectedParamToArgMapping = (0 until expectedArgCount - 1).toList

      verifyInvokedElement(invocationInfo, "Function2#apply")
      verifyArgumentsWithSingleArgList(invocationInfo, expectedArgCount, expectedProperArgsInText,
        expectedMappedParamNames, expectedPassingMechanisms, expectedParamToArgMapping)
      verifyThisExpression(invocationInfo, "local")
    }
  }

  def testLocalFunctionsWithoutParams(): Unit = {
    val sugaredSyntax = "local()"
    val desugaredSyntax1 = "local.apply"
    val desugaredSyntax2 = "local.apply()"

    val code = (invocationSyntax: String) =>
      s"""
         |object TestObject {
         |
         |  def main(): Int = {
         |  	val local = () => "Hi"
         |    ${markerStart}${invocationSyntax}${markerEnd}
         |    5
         |  }
         |}
         |
         |""".stripMargin

    for (invocationSyntax <- List(sugaredSyntax, desugaredSyntax1, desugaredSyntax2)) {
      val invocationInfo = generateInvocationInfoFor(code(invocationSyntax))

      val expectedArgCount = 1 + 0
      val expectedProperArgsInText = Nil
      val expectedMappedParamNames = Nil
      val expectedPassingMechanisms = (1 to expectedArgCount).map(_ => PassByValue).toList
      val expectedParamToArgMapping = (0 until expectedArgCount - 1).toList

      verifyInvokedElement(invocationInfo, "Function0#apply")
      verifyArgumentsWithSingleArgList(invocationInfo, expectedArgCount, expectedProperArgsInText,
        expectedMappedParamNames, expectedPassingMechanisms, expectedParamToArgMapping)
      verifyThisExpression(invocationInfo, "local")
    }
  }
}
