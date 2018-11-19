package host

import host.ir_host.MapHSeq
import ir.ArrayType
import ir.ast.fun
import lift.arithmetic.SizeVar
import org.junit.Test
import org.junit.Assert._
import opencl.ir.{Float, add, _}

class TestHost {

  val N = SizeVar("N")
  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  private def compile_native(): Unit = {

  }

  private def run_executable(): Unit = {

  }

  private def native_compile_and_run() : Unit = {

  }

  @Test
  def test_map(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/maphost"

    val f = fun( ArrayType(Float, N),
      in => MapHSeq( incrementF ) $ in
    )

    CompileHost(f, path)

    println("All done!")

  }

  @Test
  def test_reduce(): Unit = {

    println("All done!")
    assertEquals(2,0)
  }


}
