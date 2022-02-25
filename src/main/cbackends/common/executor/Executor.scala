package cbackends.common.executor

import cbackends.common.WindowsOSInUse

import scala.sys.process._
import scala.language.postfixOps

object Executor {

  private def compile_native(path: String, file: String, extra_native_compile_flags : String = "",
                             return_cmds: Boolean = false,
                             formatPathsForWindows: Boolean = false): String = {

    val full_path_file = path + "/" + file
    val target = path + "/" + "a.out"

    val openCLLink = "-lOpenCL"


    val unixCmd = (s"g++ $full_path_file -I$path" + s" -o $target -std=c++11 " + openCLLink + " " +
      extra_native_compile_flags)
    val cmd = if (formatPathsForWindows)
      unixCmd.replace("\\", "\\\\")
    else unixCmd

//    println(unixCmd)
    if (return_cmds) cmd
    else {
      val status_code = cmd !

      assert(status_code == 0, "Native Compilation error!")
      ""
    }
  }

  private def run_executable(path: String, file: String, extra_run_flags : String = "",
                             return_cmds: Boolean = false,
                             formatPathsForWindows: Boolean = false): String = {

    val unixCmd = ( if (path.isEmpty) "./" else path + "/" ) + s"$file $extra_run_flags"
    val cmd = if (formatPathsForWindows)
      unixCmd.replace("/", "\\").replace("\\", "\\\\")
    else unixCmd

//    println(unixCmd)

    if (return_cmds) cmd
    else cmd !!

  }

  def native_compile_and_run(path: String, generated_host_file: String, delete: Boolean = true,
                             test_harness_file: String = "main.cpp",
                             extra_native_compile_flags : String = "", extra_run_flags : String = "",
                             return_cmds: Boolean = false,
                             formatPathsForWindows: Boolean = false)  : String = {

    val compileCmd = compile_native(path, test_harness_file, extra_native_compile_flags, return_cmds = return_cmds)

    if(delete) {
      val status_code = if (WindowsOSInUse())
        (s"cmd.exe /S /C del $path" + "/" + s"$generated_host_file")
          .replace("\\", "\\\\")
          .replace("/", "\\\\") !
      else (s"rm -f $path" + "/" + s"$generated_host_file") !

      assert(status_code == 0, "Delete generated lib file error!")
    }

    val resultOrRunCmd = run_executable(path, "a.out", extra_run_flags, return_cmds = return_cmds)

    val status_code2 = if (WindowsOSInUse())
      ((s"cmd.exe /S /C del $path" + "\\a.out")
        .replace("\\", "\\\\")
        .replace("/", "\\\\")) !
    else (s"rm -f $path" + "/a.out") !

    assert(status_code2 == 0, "Delete generated binary error!")

    //val status_code3 = (s"rm $path" + raw"""/kernel_*.cl""".r) !

    if (return_cmds) compileCmd + " && " + resultOrRunCmd
    else resultOrRunCmd

  }
}
