package org.jetbrains.plugins.scala
package lang
package psi
package api
package expr

import org.jetbrains.plugins.scala.lang.psi.api._


import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSequenceArg, ScTypeElement}

/**
  * @author Alexander Podkhalyuzin
  *         Date: 06.03.2008
  */
trait ScTypedExpressionBase extends ScExpressionBase { this: ScTypedExpression =>
  def expr: ScExpression = findChild[ScExpression].get

  def typeElement: Option[ScTypeElement] = findChild[ScTypeElement]

  def isSequenceArg: Boolean = getLastChild.is[ScSequenceArg]

  override protected def acceptScala(visitor: ScalaElementVisitor): Unit = {
    visitor.visitTypedStmt(this)
  }
}