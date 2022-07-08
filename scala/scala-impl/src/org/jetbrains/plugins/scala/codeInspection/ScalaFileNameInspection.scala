package org.jetbrains.plugins.scala
package codeInspection

import com.intellij.codeInspection._
import com.intellij.ide.scratch.ScratchUtil
import com.intellij.lang.injection.InjectedLanguageManager
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiFile, PsiNamedElement}
import com.intellij.refactoring.RefactoringFactory
import com.intellij.refactoring.rename.RenameProcessor
import org.jetbrains.plugins.scala.console.ScalaLanguageConsole
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker

final class ScalaFileNameInspection extends LocalInspectionTool {

  import ProblemDescriptor.EMPTY_ARRAY
  import ScalaFileNameInspection._

  override def checkFile(file: PsiFile,
                         manager: InspectionManager,
                         isOnTheFly: Boolean): Array[ProblemDescriptor] = file match {
    case scalaFile: ScalaFile if canHaveErrors(scalaFile) =>
      val virtualFileName = scalaFile.getVirtualFile.getNameWithoutExtension
      findSuspiciousTypeDefinitions(scalaFile, virtualFileName)
        .map { createDescriptor(manager, scalaFile, virtualFileName, _, isOnTheFly) }
        .toArray
    case _ =>
      EMPTY_ARRAY
  }

  private def canHaveErrors(scalaFile: ScalaFile): Boolean =
    !ScalaLanguageConsole.isScalaConsoleFile(scalaFile) &&
      IntentionAvailabilityChecker.checkInspection(this, scalaFile) &&
      !InjectedLanguageManager.getInstance(scalaFile.getProject).isInjectedFragment(scalaFile) &&
      !scalaFile.isScriptFile &&
      !scalaFile.isWorksheetFile &&
      Option(scalaFile.getVirtualFile).isDefined &&
      !ScratchUtil.isScratch(scalaFile.getVirtualFile)

  private def findSuspiciousTypeDefinitions(scalaFile: ScalaFile, virtualFileName: String): Seq[ScTypeDefinition] = {
    val members = scalaFile.members
    val (definitions, others) = members.partition(_.is[ScTypeDefinition])
    val typeDefinitions = definitions.asInstanceOf[Seq[ScTypeDefinition]]

    if (others.nonEmpty || typeDefinitions.size > 2)
      Seq.empty
    else if (typeDefinitions.forall { p => ScalaNamesUtil.equivalent(p.name, virtualFileName) })
      Seq.empty
    else if (scalaFile.name == "package.scala" && typeDefinitions.exists {
      case scalaObject: ScObject if scalaObject.isPackageObject => true
      case _ => false
    })
      Seq.empty
    else {
      val hasObject = typeDefinitions.exists {
        case _: ScObject => true
        case _ => false
      }
      if ((hasObject && typeDefinitions.size == 2) || typeDefinitions.size == 1)
        typeDefinitions.filter { p => !ScalaNamesUtil.equivalent(p.name, virtualFileName) }
      else
        Seq.empty
    }
  }

  private def createDescriptor(manager: InspectionManager, scalaFile: ScalaFile, virtualFileName: String, clazz: ScTypeDefinition, isOnTheFly: Boolean) = {
    val localQuickFixes = Array[LocalQuickFix](
      new RenameClassQuickFix(clazz, virtualFileName),
      new RenameFileQuickFix(scalaFile, clazz.name + "." + ScalaFileType.INSTANCE.getDefaultExtension)
    )

    manager.createProblemDescriptor(clazz.nameId,
      getDisplayName,
      isOnTheFly,
      localQuickFixes,
      ProblemHighlightType.GENERIC_ERROR_OR_WARNING
    )
  }
}

object ScalaFileNameInspection {
  private abstract sealed class RenameQuickFixBase[T <: PsiNamedElement](element: T,
                                                                         name: String,
                                                                         override final val getFamilyName: String)
    extends AbstractFixOnPsiElement(
      ScalaInspectionBundle.message("fileName.rename.text", getFamilyName, element.name, name),
      element
    ) {

    override protected final def doApplyFix(element: T)
                                           (implicit project: Project): Unit = invokeLater {
      onElement(element)
    }

    protected def onElement(element: T)
                           (implicit project: Project): Unit
  }

  private final class RenameClassQuickFix(clazz: ScTypeDefinition, name: String)
    extends RenameQuickFixBase(clazz, name, ScalaInspectionBundle.message("fileName.rename.class")) {

    override protected def onElement(clazz: ScTypeDefinition)
                                    (implicit project: Project): Unit =
      RefactoringFactory.getInstance(project).createRename(clazz, name).run()
  }

  private final class RenameFileQuickFix(file: ScalaFile, name: String)
    extends RenameQuickFixBase(file, name, ScalaInspectionBundle.message("fileName.rename.file")) {

    override protected def onElement(file: ScalaFile)
                                    (implicit project: Project): Unit =
      new RenameProcessor(project, file, name, false, false).run()
  }

}