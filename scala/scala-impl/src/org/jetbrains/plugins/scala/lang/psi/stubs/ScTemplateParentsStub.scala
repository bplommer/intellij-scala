package org.jetbrains.plugins.scala
package lang
package psi
package stubs

import com.intellij.psi.stubs.StubElement
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateParents

trait ScTemplateParentsStub extends StubElement[ScTemplateParents] {
  def parentTypesTexts: Array[String]

  def parentTypeElements: Seq[ScTypeElement]

  def constructorText: Option[String]

  def supersText: String = (constructorText ++: parentTypesTexts).mkString(" with ")
}