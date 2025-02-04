package org.jetbrains.sbt
package project.structure

import com.intellij.openapi.util.io.FileUtil

import java.io.File
import scala.jdk.CollectionConverters._

/**
  * Support for the .jvmopts file loaded by the sbt launcher script as alternative to command line options.
  */
object JvmOpts {

  def loadFrom(directory: File): Seq[String] = {
    val jvmOptsFile = directory / ".jvmopts"
    if (jvmOptsFile.exists && jvmOptsFile.isFile && jvmOptsFile.canRead)
      FileUtil.loadLines(jvmOptsFile)
        .asScala.iterator
        .map(_.trim)
        .filter(_.startsWith("-"))
        .toSeq
    else
      Seq.empty
  }

}
