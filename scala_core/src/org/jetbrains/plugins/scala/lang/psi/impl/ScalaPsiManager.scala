package org.jetbrains.plugins.scala.lang.psi.impl

import api.statements.params.ScTypeParam
import com.intellij.util.containers.WeakHashMap
import com.intellij.openapi.project.Project
import com.intellij.openapi.components.ProjectComponent
import com.intellij.psi.impl.PsiManagerEx
import com.intellij.psi.{PsiManager, PsiTypeParameter}
import types._

class ScalaPsiManager(project: Project) extends ProjectComponent {
  def projectOpened {}
  def projectClosed {}
  def getComponentName = "ScalaPsiManager"
  def disposeComponent {}
  def initComponent {
    PsiManager.getInstance(project).asInstanceOf[PsiManagerEx].registerRunnableToRunOnAnyChange(new Runnable {
      override def run = typeVariables.clear
    })
  }

  private val typeVariables = new WeakHashMap[PsiTypeParameter, ScTypeVariable]

  def typeVariable(tp: PsiTypeParameter) : ScTypeVariable = {
    val existing = typeVariables.get(tp)
    if (existing != null) existing else {
      synchronized {
        val tv = tp match {
          case stp: ScTypeParam => {
            val inner = stp.typeParameters.map{typeVariable(_)}.toList
            synchronized{
              typeVariables.put(tp, new ScTypeVariable("", inner, Nothing, Any)) //temp put to avoid SOE
              val lower = stp.lowerBound
              val upper = stp.upperBound
              new ScTypeVariable(stp.name, inner, lower, upper)
            }
          }
          case _ => {
            val supers = tp.getSuperTypes
            val scalaSuper = supers match {
              case Array(single) => ScType.create(single, project)
              case many => new ScCompoundType(many.map{ScType.create(_, project)}, Seq.empty, Seq.empty)
            }
            new ScTypeVariable(tp.getName, Nil, Nothing, scalaSuper)
          }
        }
        typeVariables.put(tp, tv)
        tv
      }
    }
  }
}

object ScalaPsiManager {
  def instance(project : Project) = project.getComponent(classOf[ScalaPsiManager])

  def typeVariable(tp : PsiTypeParameter) = instance(tp.getProject).typeVariable(tp)
}