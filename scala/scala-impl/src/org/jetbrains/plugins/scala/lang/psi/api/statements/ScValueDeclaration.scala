package org.jetbrains.plugins.scala
package lang
package psi
package api
package statements

import org.jetbrains.plugins.scala.lang.psi.api._


import org.jetbrains.plugins.scala.lang.psi.api.base.ScIdList
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition

/**
* @author Alexander Podkhalyuzin
* Date: 22.02.2008
* Time: 9:44:29
*/

trait ScValueDeclarationBase extends ScValueBase with ScTypedDeclarationBase { this: ScValueDeclaration =>
  def getIdList: ScIdList
  override def declaredElements : Seq[ScTypedDefinition]
  override def isAbstract: Boolean = true
}