package host

import host.ir_host.MapHSeq
import ir.ArrayType
import ir.ast.fun
import lift.arithmetic.SizeVar
import org.junit.Test
import org.junit.Assert._
import opencl.ir.{Float, add, _}
import sys.process._

class TestHost {

  val N = SizeVar("N")
  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  private def compile_native(path: String, file: String): Unit = {

    val full_path_file = path + "/" + file
    val target = path + "/" + "a.out"

    val status_code = s"g++ $full_path_file -I$path -o $target" !

    assert(status_code == 0, "Native Compilation error!")


  }

  private def run_executable(path: String, file: String): String = {

    ( ( if(path.isEmpty()) "./" else path + "/" ) + s"$file" ) !!

  }

  private def native_compile_and_run(path: String, file: String)  : String = {

    compile_native(path, "main.cpp")

    val status_code = (s"rm $path" + "/" + s"$file") !

    assert(status_code == 0, "Delete generated lib file error!")

    val result = run_executable(path, "a.out")

    val status_code2 = (s"rm $path" + "/a.out") !

    assert(status_code2 == 0, "Delete generated lib file error!")

    result


  }

  @Test
  def test_map(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/maphost"
    val file = "libmap.cpp"

    val f = fun( ArrayType(Float, N),
      in => MapHSeq( incrementF ) $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"

    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_reduce(): Unit = {

    println("All done!")
    assertEquals(2,0)
  }


}
