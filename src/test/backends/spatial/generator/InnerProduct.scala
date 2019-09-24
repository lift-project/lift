package backends.spatial.generator

import backends.c
import backends.c.host.host_ir.{OclFunc, ToGPU, ToHost}
import backends.spatial
import backends.spatial.accel.ir.pattern.MapPar
import backends.spatial.host
import backends.spatial.host.ir.ast.AccelFun
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

  val N = 1024
  val input: Array[Float] = (0 until 16).toArray.map(_.toFloat)
  val gold: Float = (input, input).zipped.map(_*_).sum

  val commonCodeOutputPath = System.getProperty("user.dir") + "/src/test/backends.spatial/host"

  val scalarDotLambda: Lambda = fun(
    ArrayTypeWSWC(Float, N),
    ArrayTypeWSWC(Float, N),
    (x, y) =>
      toGlobal(MapSeq(id)) o
        ReduceSeq(add, 0.0f) o
        MapSeq(mult) $ Zip(x, y)
  )

  @Test
  def openclDotProduct(): Unit = {
//    val code = opencl.executor.Compile(scalarDotLambda)
//
//    val (output, _) = Execute(1, 1)[Array[Float]](code, scalarDotLambda, input, input)
//    println("OUT: " + output.head.toString)

    val codeOutputPath = s"$commonCodeOutputPath/00.OpenCLScalarDot"
    val hostCodeFileName = "opencl_scalar_dot_host.cpp"

    val hostingLambda = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (x, y) =>
        ToHost() $ OclFunc(scalarDotLambda, cpu_timer = true, gpu_timer = true)(ToGPU(x), ToGPU(y))
    )

    c.global.GlobalCompiler(hostingLambda, codeOutputPath, List(hostCodeFileName))

    val actualOutput: String = backends.c.common.executor.Executor.native_compile_and_run(
      codeOutputPath, hostCodeFileName)

    print(actualOutput)

    val pattern = raw"(?:.*\n)+(\d+).*\n".r

    val pattern(count) = actualOutput.stripMargin
    assertEquals(gold, count.toFloat, 0.001f)
  }

  @Test
  def spatialDotProduct(): Unit = {

    val expectedOutCode = """
       Accel {
        // Allocate local SRAMs
        val s1 = SRAM[T](len)
        val s2 = SRAM[T](len)

        // Transfer data
        s1 load d1
        s2 load d2
  
        // Multiply and accumulate
        out := Reduce(Reg[T](0))(len by 1) { i =>
          s1(i) * s2(i)
        }(_ + _)
      }"""

    val runTimeLambda: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (x, y) =>
        host.ir.ast.AccelFun(scalarDotLambda)(x, y)
    )

    val out = spatial.runtime.RuntimeBuilder(runTimeLambda)
    
  }

  val tileSize = 32
  val outerParFactor = 2
  val innerParFactor = 16

  val reduceTile: Lambda = fun(
    TupleType(
      ArrayTypeWSWC(Float, tileSize),
      ArrayTypeWSWC(Float, tileSize)
    ), tile =>
      ReduceSeq(add, 0.0f) o
        MapSeq(mult) o
        fun((a, b) => Zip(
          /*Parallel(*/
          Tuple(
            MapPar(MapLcl(toLocal(id))) o Split(innerParFactor) $ a,
            MapPar(MapLcl(toLocal(id))) o Split(innerParFactor) $ b)
          /*)*/ ))
        o Unzip() $ tile)

  val scalarDotLambdaTiled: Lambda = fun(
    ArrayTypeWSWC(Float, N),
    ArrayTypeWSWC(Float, N),
    (x, y) =>
      toGlobal(MapSeq(id)) o
        /* Reduce all parallel groups */
        ReduceSeq(add, Value(0, Float)) o
        MapSeq(fun(parGroupOfTiles =>
          /* Reduce each parallel group */
          ReduceSeq(add, Value(0, Float)) o
            Join() o
            /* Reduce each tile of a parallel group in parallel */
            MapPar(fun(tile =>
              /* Reduce one tile of a parallel group */
              ReduceSeq(reduceTile, Value(0, Float)) $
                tile)) $
            parGroupOfTiles)) o
        Split(outerParFactor) o
        Split(tileSize) $ Zip(x, y)
  )

  @Test
  def spatialDotProductTiled(): Unit = {

    val expectedOutCode = """
      Accel {
        out := Reduce(Reg[T](0.to[T]))(N by tileSize par outerParFactor){i =>
          val aBlk = SRAM[T](tileSize)
          val bBlk = SRAM[T](tileSize)
          Parallel {
            aBlk load a(i::i+tileSize par innerParFactor)
            bBlk load b(i::i+tileSize par innerParFactor)
          }
          Reduce(Reg[T](0.to[T]))(ts par innerParFactor){ii => aBlk(ii) * bBlk(ii) }{_+_}
        }{_+_}
      }
    """
  }
}
