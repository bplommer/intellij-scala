package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package elements
package signatures

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{StubElement, StubInputStream, StubOutputStream}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameters
import org.jetbrains.plugins.scala.lang.psi.impl.statements.params.ScParametersImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScParamClausesStubImpl

/**
 * User: Alexander Podkhalyuzin
 * Date: 19.10.2008
 */

class ScParamClausesElementType extends ScStubElementType[ScParamClausesStub, ScParameters]("parameter clauses") {
  def serialize(stub: ScParamClausesStub, dataStream: StubOutputStream) {}

  override def deserialize(dataStream: StubInputStream, parentStub: StubElement[_ <: PsiElement]): ScParamClausesStub =
    new ScParamClausesStubImpl(parentStub.asInstanceOf[StubElement[PsiElement]], this)

  def createStubImpl[ParentPsi <: PsiElement](psi: ScParameters, parentStub: StubElement[ParentPsi]): ScParamClausesStubImpl[ParentPsi] = {
    new ScParamClausesStubImpl(parentStub, this)     
  }

  override def createPsi(stub: ScParamClausesStub): ScParameters = new ScParametersImpl(stub)

  override def createElement(node: ASTNode): ScParameters = new ScParametersImpl(node)
}