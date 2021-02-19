package org.jetbrains.plugins.scala
package lang
package psi
package api
package base
package types

import org.jetbrains.plugins.scala.lang.psi.api._


import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

/** 
* @author Alexander Podkhalyuzin
* Date: 07.03.2008
*/

trait ScAnnotTypeElementBase extends ScTypeElementBase { this: ScAnnotTypeElement =>
  override protected val typeName = "TypeWithAnnotation"

  def typeElement: ScTypeElement = findChild[ScTypeElement].get

  override protected def innerType: TypeResult = typeElement.`type`()
}