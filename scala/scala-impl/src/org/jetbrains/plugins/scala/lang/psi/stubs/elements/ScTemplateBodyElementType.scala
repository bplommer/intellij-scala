package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package elements

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{StubElement, StubInputStream}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.templates.ScTemplateBodyImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScTemplateBodyStubImpl

class ScTemplateBodyElementType extends ScStubElementType[ScTemplateBodyStub, ScTemplateBody]("template body") {
  override def deserialize(dataStream: StubInputStream, parentStub: StubElement[_ <: PsiElement]): ScTemplateBodyStub =
    new ScTemplateBodyStubImpl(parentStub, this)

  override def createStubImpl(templateBody: ScTemplateBody, parentStub: StubElement[_ <: PsiElement]): ScTemplateBodyStub =
    new ScTemplateBodyStubImpl(parentStub, this)

  override def createElement(node: ASTNode): ScTemplateBody = new ScTemplateBodyImpl(node)

  override def createPsi(stub: ScTemplateBodyStub): ScTemplateBody = new ScTemplateBodyImpl(stub)
}