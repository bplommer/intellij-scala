package org.jetbrains.plugins.scala
package lang
package psi
package api
package toplevel

import org.jetbrains.plugins.scala.lang.psi.api._


import com.intellij.psi.scope.PsiScopeProcessor
import com.intellij.psi.{PsiElement, ResolveState}

/**
 * @author ilyas
 */

trait ScImportableDeclarationsOwnerBase extends ScalaPsiElementBase { this: ScTypedDefinition with ScImportableDeclarationsOwner =>

  /**
   * Declarations may be taken from stable elements only
   */
  override def processDeclarations(processor: PsiScopeProcessor, state: ResolveState, lastParent: PsiElement, place: PsiElement): Boolean =
    if (isStable) {
      ScalaPsiUtil.processImportLastParent(processor, state, place, lastParent, `type`())
    } else true

}