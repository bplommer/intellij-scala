package org.jetbrains.plugins.scala
package lang
package psi
package api
package base
package patterns

import org.jetbrains.plugins.scala.lang.psi.api._


import com.intellij.psi.PsiElement
import com.intellij.psi.tree.TokenSet
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScExpression, ScGuard}

/** 
* @author Alexander Podkhalyuzin
* Date: 28.02.2008
*/

trait ScCaseClauseBase extends ScalaPsiElementBase { this: ScCaseClause =>
  def pattern: Option[ScPattern] = findChild[ScPattern]
  def expr: Option[ScExpression] = findChild[ScExpression]
  def guard: Option[ScGuard] = findChild[ScGuard]
  def funType: Option[PsiElement] = {
    val result = getNode.getChildren(TokenSet.create(ScalaTokenTypes.tFUNTYPE, 
      ScalaTokenTypes.tFUNTYPE_ASCII))
    if (result.length != 1) None
    else Some(result(0).getPsi)
  }
  override protected def acceptScala(visitor: ScalaElementVisitor): Unit = {
    visitor.visitCaseClause(this)
  }
}

object ScCaseClauseBase {
  implicit class ScCaseClauseExt(private val cc: ScCaseClause) extends AnyVal {
    def resultExpr: Option[ScExpression] =
      cc.expr match {
        case Some(block: ScBlock) =>  block.resultExpression
        case _ => None
      }
  }
}

abstract class ScCaseClauseCompanion {
  def unapply(e: ScCaseClause): Option[(Option[ScPattern], Option[ScGuard], Option[ScExpression])] =
    Option(e).map(e => (e.pattern, e.guard, e.expr))
}