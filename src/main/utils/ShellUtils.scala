package utils

import java.io.IOException
import java.nio.file.{Files, Paths}

import cbackends.common.WindowsOSInUse

import scala.language.postfixOps
import scala.sys.process._

object ShellUtils {
  def mkDirTree(path: String, overwriteIfExists: Boolean = false): Unit = {
    val formattedPath = osFormattedPath(path)

    (overwriteIfExists, Files.exists(Paths.get(formattedPath))) match {
      case (false, true)  => throw new IOException(s"Cannot create directory -- it already exists: $formattedPath")
      case (true, true)   => if (!WindowsOSInUse()) (s"rm -rf $formattedPath") !
                             else (s"cmd.exe /S /C rmdir /s /q $formattedPath") !
      case _ =>
    }
    if (!overwriteIfExists && Files.exists(Paths.get(formattedPath)))
      throw new IOException(s"Cannot create directory -- it already exists: $formattedPath")

    if (!WindowsOSInUse()) (s"mkdir -p $formattedPath") !
    else (s"cmd.exe /S /C md $formattedPath >nul 2>&1") !
  }

  def osFormattedPath(path: String): String =
    if (WindowsOSInUse()) path.replace("/", "\\")
    else path

  def join(paths: String*): String = paths.mkString(if (WindowsOSInUse()) "\\" else "/")
}
