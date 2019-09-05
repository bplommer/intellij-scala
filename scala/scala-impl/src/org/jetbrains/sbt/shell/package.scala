package org.jetbrains.sbt

import com.intellij.openapi.wm.ToolWindow
import com.intellij.ui.content.Content
import org.jetbrains.annotations.NotNull

package object shell {

  implicit class ToolWindowExt(private val toolWindow: ToolWindow) extends AnyVal {

    def setContent(@NotNull content: Content): Unit = {
      val twContentManager = toolWindow.getContentManager
      twContentManager.removeAllContents(true)
      twContentManager.addContent(content)
    }
  }
}
