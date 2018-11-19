package host

import host.ir_host.MapHSeq
import ir.ArrayType
import ir.ast.fun
import lift.arithmetic.SizeVar
import org.junit.Test
import opencl.ir.{Float, add, _}

class TestHost {

  val N = SizeVar("N")
  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  @Test
  def test_map(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/maphost"

    val f = fun( ArrayType(Float, N),
      in => MapHSeq( incrementF ) $ in
    )

    CompileHost(f, path)

    println("All done!")

  }

}
