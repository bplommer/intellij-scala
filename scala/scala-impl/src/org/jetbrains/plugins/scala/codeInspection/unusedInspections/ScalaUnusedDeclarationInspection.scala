package org.jetbrains.plugins.scala.codeInspection.unusedInspections

import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.codeInspection.deadCode.UnusedDeclarationInspectionBase
import com.intellij.openapi.roots.TestSourcesFilter
import com.intellij.psi._
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.ScalaInspectionBundle
import org.jetbrains.plugins.scala.codeInspection.ui.InspectionOptionsComboboxPanel
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaModifier
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil.{inNameContext, isOnlyVisibleInLocalFile, superValsSignatures}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSelfTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScModifierListOwner, ScNamedElement}
import org.jetbrains.plugins.scala.lang.psi.impl.search.ScalaOverridingMemberSearcher
import org.jetbrains.plugins.scala.project.{ModuleExt, ScalaLanguageLevel}
import org.jetbrains.plugins.scala.util.SAMUtil.PsiClassToSAMExt
import org.jetbrains.plugins.scala.util.ScalaMainMethodUtil

import javax.swing.JComponent
import scala.beans.{BeanProperty, BooleanBeanProperty}

final class ScalaUnusedDeclarationInspection extends HighlightingPassInspection {

  import ScalaUnusedDeclarationInspection._
  import org.jetbrains.plugins.scala.codeInspection.ui.CompilerInspectionOptions._

  override def isEnabledByDefault: Boolean = true

  override def getDisplayName: String = ScalaInspectionBundle.message("display.name.unused.declaration")

  @BooleanBeanProperty
  var reportPublicDeclarations: Boolean = true

  @BeanProperty
  var reportLocalDeclarations: Int = 0

  override def createOptionsPanel: JComponent = {
    val panel = new InspectionOptionsComboboxPanel(this)
    panel.addCheckbox(
      ScalaInspectionBundle.message("name.unused.declaration.report.public.declarations"),
      "reportPublicDeclarations"
    )
    panel.addComboboxForCompilerOption(
      label = ScalaInspectionBundle.message("name.unused.declaration.report.local.declarations"),
      getSelectedIndex = () => reportLocalDeclarations,
      setSelectedIndex = reportLocalDeclarations = _
    )
    panel
  }

  override def invoke(element: PsiElement, isOnTheFly: Boolean): Seq[ProblemInfo] = {

    if (!shouldProcessElement(element)) {
      Seq.empty
    } else {

      // Structure to encapsulate the possibility to check a delegate element to determine
      // usedness of the original PsiElement passed into `invoke`.
      // When a ProblemInfo is created, we still want it to pertain to the original element under inspection,
      // that was passed into `invoke`, and not to the delegate.
      // So we use the delegate only to determine usedness, and the original for all other operations.
      case class InspectedElement(original: ScNamedElement, delegate: ScNamedElement)

      val elements: Seq[InspectedElement] = element match {
        case functionDeclaration: ScFunctionDeclaration
          if Option(functionDeclaration.getContainingClass).exists(_.isSAMable) =>
          Option(functionDeclaration.getContainingClass).toSeq
            .collect { case named: ScNamedElement => named }
            .map(InspectedElement(functionDeclaration, _))
        case named: ScNamedElement => Seq(InspectedElement(named, named))
        case _ => Seq.empty
      }

      elements.flatMap {
        case InspectedElement(_, _: ScTypeParam) if !isOnTheFly => Seq.empty
        case InspectedElement(_, typeParam: ScTypeParam) if typeParam.hasBounds || typeParam.hasImplicitBounds => Seq.empty
        case InspectedElement(_, inNameContext(holder: PsiAnnotationOwner)) if hasUnusedAnnotation(holder) =>
          Seq.empty
        case InspectedElement(original: ScNamedElement, delegate: ScNamedElement)
          if CheapRefSearcher.search(delegate, isOnTheFly, reportPublicDeclarations).isEmpty =>

          val dontReportPublicDeclarationsQuickFix =
            if (isOnlyVisibleInLocalFile(original)) None else Some(new DontReportPublicDeclarationsQuickFix(original))

          val addScalaAnnotationUnusedQuickFix = if (delegate.scalaLanguageLevelOrDefault < ScalaLanguageLevel.Scala_2_13)
            None else Some(new AddScalaAnnotationUnusedQuickFix(original))

          val message = if (isOnTheFly) {
            ScalaUnusedDeclarationInspection.annotationDescription
          } else {
            UnusedDeclarationVerboseProblemInfoMessage(original)
          }

          Seq(
            ProblemInfo(
              original.nameId,
              message,
              ProblemHighlightType.LIKE_UNUSED_SYMBOL,
              DeleteUnusedElementFix.quickfixesFor(original) ++
                dontReportPublicDeclarationsQuickFix ++
                addScalaAnnotationUnusedQuickFix
            )
          )
        case _ =>
          Seq.empty
      }
    }
  }

  override def shouldProcessElement(elem: PsiElement): Boolean =
    elem match {
      case n: ScNamedElement =>
        n.nameContext match {
          case m: ScMember if m.isLocal && reportLocalDeclarations == AlwaysDisabled =>
            false
          case m: ScMember if m.isLocal && reportLocalDeclarations == ComplyToCompilerOption =>
            val compilerOptions = n.module.toSeq.flatMap(_.scalaCompilerSettings.additionalCompilerOptions)
            compilerOptions.contains("-Wunused:locals") || compilerOptions.contains("-Wunused:linted") || compilerOptions.contains("-Xlint:unused")
          case _ => true
        }
      case _ => true
    }
}

object ScalaUnusedDeclarationInspection {
  @Nls
  val annotationDescription: String = ScalaInspectionBundle.message("declaration.is.never.used")

  private def hasOverrideModifier(member: ScModifierListOwner): Boolean =
    member.hasModifierPropertyScala(ScalaModifier.OVERRIDE)

  private def isOverridingOrOverridden(element: PsiNamedElement): Boolean =
    superValsSignatures(element, withSelfType = true).nonEmpty || isOverridden(element)

  private def isOverridingFunction(func: ScFunction): Boolean =
    hasOverrideModifier(func) || func.superSignatures.nonEmpty || isOverridden(func)

  private def isOverridden(member: PsiNamedElement): Boolean =
    ScalaOverridingMemberSearcher.search(member, deep = false, withSelfType = true).nonEmpty

  private def hasUnusedAnnotation(holder: PsiAnnotationOwner): Boolean =
    holder.hasAnnotation("scala.annotation.unused") ||
      // not entirely correct, but if we find @nowarn here in this situation
      // we can assume that it is directed at the unusedness of the symbol
      holder.hasAnnotation("scala.annotation.nowarn")
}
