package org.jetbrains.plugins.scala
package lang
package psi
package api
package toplevel
package templates

import org.jetbrains.plugins.scala.lang.psi.api._
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScBraceOwner, ScBraceOwnerBase}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSelfTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScDeclaredElementsHolder, ScFunction, ScTypeAlias, ScValueOrVariable}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._

/**
 * @author Alexander Podkhalyuzin
 *         Date: 22.02.2008
 *         Time: 9:38:36
 */
trait ScTemplateBodyBase extends ScalaPsiElementBase with ScControlFlowOwnerBase with ScBraceOwnerBase { this: ScTemplateBody =>

  def members: Seq[ScMember]

  def holders: Seq[ScDeclaredElementsHolder]

  def functions: Seq[ScFunction]

  def properties: Seq[ScValueOrVariable]

  def aliases: Seq[ScTypeAlias]

  def typeDefinitions: Seq[ScTypeDefinition]

  def exprs: Seq[ScExpression]

  def selfTypeElement: Option[ScSelfTypeElement]
}