package spatial.generator

import cbackends.global.GlobalCompiler
import cbackends.host.host_ir.{OclFunc, ToGPU, ToHost}
import ir._
import ir.ast._
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object InnerProduct extends TestWithExecutor

class InnerProduct {

  val N = 16
  val input: Array[Float] = (0 until 16).toArray.map(_.toFloat)
  val gold: Float = (input, input).zipped.map(_*_).sum

  val commonCodeOutputPath = System.getProperty("user.dir") + "/src/test/spatial/host"

  val scalarDotLambda: Lambda = fun(
    ArrayTypeWSWC(Float, 16),
    ArrayTypeWSWC(Float, 16),
    (x, y) =>
      toGlobal(MapSeq(id)) o
        ReduceSeq(add, 0.0f) o
        MapSeq(mult) $ Zip(x, y)
  )

  @Test
  def openclTest(): Unit = {
//    val code = opencl.executor.Compile(scalarDotLambda)
//
//    val (output, _) = Execute(1, 1)[Array[Float]](code, scalarDotLambda, input, input)
//    println("OUT: " + output.head.toString)

    val codeOutputPath = s"$commonCodeOutputPath/00.OpenCLScalarDot"
    val hostCodeFileName = "opencl_scalar_dot_host.cpp"

    val hostingLambda = fun(
      ArrayTypeWSWC(Float, 16),
      ArrayTypeWSWC(Float, 16),
      (x, y) =>
        ToHost() $ OclFunc(scalarDotLambda)(ToGPU(x), ToGPU(y))
    )

    GlobalCompiler ! (hostingLambda, codeOutputPath, List(hostCodeFileName))

    val actualOutput: String = cbackends.common.executor.Executor.native_compile_and_run(
      codeOutputPath, hostCodeFileName)

    println(actualOutput)

//    assertEquals(gold, output.head, 0.001f)
  }

  @Test
  def spatialAccelTest(): Unit = {
    val expectedOutCode = """
       Accel {
        // Allocate local SRAMs
        val s1 = SRAM[T](len)
        val s2 = SRAM[T](len)

        // Transfer data
        s1 load d1
        s2 load d2
  
        // Multiply and accumulate
        x := Reduce(Reg[T](0))(len by 1) { i =>
          s1(i) * s2(i)
        }(_ + _)
      }"""

//    val code = spatial.
    
  }
}
