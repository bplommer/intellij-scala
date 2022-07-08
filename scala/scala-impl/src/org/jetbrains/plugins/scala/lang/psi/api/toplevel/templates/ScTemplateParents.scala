package org.jetbrains.plugins.scala
package lang
package psi
package api
package toplevel
package templates

import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructorInvocation
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.types.ScType

trait ScTemplateParents extends ScalaPsiElement {

  def typeElements: Seq[ScTypeElement]

  def superTypes: Seq[ScType]

  def supersText: String

  def allTypeElements: Seq[ScTypeElement]

  final def constructorInvocation: Option[ScConstructorInvocation] = findChild[ScConstructorInvocation]

  final def constructorInvocations: Seq[ScConstructorInvocation] = findChildren[ScConstructorInvocation]

  final def typeElementsWithoutConstructor: Seq[ScTypeElement] = findChildren[ScTypeElement]
}
