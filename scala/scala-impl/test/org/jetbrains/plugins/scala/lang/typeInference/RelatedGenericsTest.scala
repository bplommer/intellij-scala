package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

class RelatedGenericsTest extends ScalaLightCodeInsightFixtureTestAdapter {

  def testSCL9347(): Unit = checkTextHasNoErrors(
    """
      |object SCL9347 {
      |  trait Record
      |  trait MutableRecord[TR <: Record] {
      |    def table: Table[TR, MutableRecord[TR]]
      |  }
      |
      |  // Important: MTR must be related to TR
      |  class Table[TR <: Record, MTR <: MutableRecord[TR]] {selfTable =>
      |    class Field {
      |      def table = selfTable
      |    }
      |  }
      |
      |  type AnyTable = Table[TR, _ <: MutableRecord[TR]] forSome {type TR <: Record}
      |
      |  def needTable(table: AnyTable) = ???
      |
      |  def foo(field: AnyTable#Field) = {
      |    field.table // Good code red
      |  }
      |
      |  def foo2(field: Table[TR, MTR]#Field forSome {type TR <: Record; type MTR <: MutableRecord[TR]}) = {
      |    field.table // Good code red
      |  }
      |
      |  def bar(mutableRecord: MutableRecord[_ <: Record]) {
      |    needTable(mutableRecord.table) // Good code red
      |  }
      |}
    """.stripMargin)
}
