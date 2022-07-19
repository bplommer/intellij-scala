package org.jetbrains.plugins.scala
package codeInspection
package typeAnnotation

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFunctionExpr, ScTypedExpression, ScUnderscoreSection}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScModifierListOwner
import org.jetbrains.plugins.scala.settings.annotations._
import org.jetbrains.plugins.scala.util._

import scala.annotation.nowarn

/**
  * Pavel Fatin
  */
@nowarn("msg=" + AbstractInspection.DeprecationText)
class TypeAnnotationInspection extends AbstractInspection {
  import TypeAnnotationInspection._

  override def actionFor(implicit holder: ProblemsHolder, isOnTheFly: Boolean): PartialFunction[PsiElement, Unit] = {
    case value: ScPatternDefinition if value.isSimple && !value.hasExplicitType =>
      inspect(value, value.bindings.head, value.expr)
    case variable: ScVariableDefinition if variable.isSimple && !variable.hasExplicitType =>
      inspect(variable, variable.bindings.head, variable.expr)
    case method: ScFunctionDefinition if method.hasAssign && !method.hasExplicitType && !method.isConstructor =>
      inspect(method, method.nameId, method.body)
    case (parameter: ScParameter) && Parent(Parent(Parent(_: ScFunctionExpr))) if parameter.typeElement.isEmpty =>
      inspect(parameter, parameter.nameId, implementation = None)
    case (underscore: ScUnderscoreSection) && Parent(parent) if underscore.getTextRange.getLength == 1 &&
      !parent.isInstanceOf[ScTypedExpression] && !parent.isInstanceOf[ScFunctionDefinition] &&
      !parent.isInstanceOf[ScPatternDefinition] && !parent.isInstanceOf[ScVariableDefinition] =>
      inspect(underscore, underscore, implementation = None)
  }
}

object TypeAnnotationInspection {

  def getReasonForTypeAnnotationOn(element: ScalaPsiElement, implementation: Option[ScExpression]): Option[String] = {
    val declaration = Declaration(element)
    val location = Location(element)

    ScalaTypeAnnotationSettings(element.getProject).reasonForTypeAnnotationOn(
      declaration, location, implementation.map(Expression))
  }

  private def inspect(element: ScalaPsiElement,
                      anchor: PsiElement,
                      implementation: Option[ScExpression])
                     (implicit holder: ProblemsHolder): Unit = {

    getReasonForTypeAnnotationOn(element, implementation).foreach { reason =>

      val fixes =
          Seq(new AddTypeAnnotationQuickFix(anchor), new ModifyCodeStyleQuickFix())

      holder.registerProblem(anchor, ScalaInspectionBundle.message("type.annotation.required.for", reason), fixes: _*)
    }
  }

  @nowarn("cat=deprecation")
  private class ModifyCodeStyleQuickFix extends LocalQuickFixBase(ScalaInspectionBundle.message("quickfix.modify.code.style")) {
    override def applyFix(project: Project, problemDescriptor: ProblemDescriptor): Unit =
      TypeAnnotationUtil.showTypeAnnotationsSettings(project)
  }
}
