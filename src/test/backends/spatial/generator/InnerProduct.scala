package backends.spatial.generator

import arithmetic.TypeVar
import backends.c.host.host_ir.{OclFunc, ToGPU, ToHost}
import backends.{Backend, c}
import ir._
import ir.ast._
import ir.ast.debug.AssertType
import lift.arithmetic.SizeVar
import opencl.executor.TestWithExecutor
import org.junit.Assert._
import org.junit.{AfterClass, Test}

object InnerProduct extends TestWithExecutor {
  @AfterClass override def after(): Unit = {
    Backend.setOpenCL()
    super.after()
  }
}

class InnerProduct {

  @Test
  def spatialDotProductTiled(): Unit = {
    import backends.spatial.accel.ir._
    import backends.spatial.accel.ir.pattern._
    import backends.spatial.common.ir._
    import backends.spatial.host.ir.ast.AccelFun

    Backend.setSpatial()

    val N = SizeVar("N")
    val tileSize = SizeVar("tileSize")
    val outerParFactor = SizeVar("outerParFactor")
    val innerParFactor = SizeVar("innerParFactor")
//    val N = 1024
//    val tileSize = 32
//    val outerParFactor = 2
//    val innerParFactor = 16

//    The Spatial code we are looking to generate ideally:
//      Accel {
//        out := Reduce(Reg[T](0.to[T]))(N by tileSize par outerParFactor){i =>
//          val aBlk = SRAM[T](tileSize)
//          val bBlk = SRAM[T](tileSize)
//          Parallel {
//            aBlk load a(i::i+tileSize par innerParFactor)
//            bBlk load b(i::i+tileSize par innerParFactor)
//          }
//          Reduce(Reg[T](0.to[T]))(ts par innerParFactor){ii => aBlk(ii) * bBlk(ii) }{_+_}
//        }{_+_}
//      }

    val x = TypeVar()

    val id = UserFun("id", Array("x"), "x", Seq(Float), Float)
    val id1D = UserFun("id", Array("x"), "x", Seq(ArrayType(Float, x)), ArrayType(Float, x))


    val scalaDotLambdaTiled: Lambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, b) =>
        AssertType(Float, "Dot product output type") o

          toArgOut(id) o
          SpPipeFold(chunkSize = tileSize, stride = tileSize, factor = outerParFactor,
            fMap = fun(
              ArrayType(TupleType(Float, Float), tileSize), tileAB => {

                val tileABsram = tileAB :>> Parallel(fun(tileABpiped => {
                  val tileA = Get(Unzip() $ tileABpiped, 0)
                  val tileB = Get(Unzip() $ tileABpiped, 1)

                  Zip(
                    toSRAM(BurstUserFun(id1D, innerParFactor)) $ tileA,
                    toSRAM(BurstUserFun(id1D, innerParFactor)) $ tileB)
                }))

                SpPipeFold(chunkSize = 1, stride = 1, factor = innerParFactor,
                  fMap = backends.spatial.accel.ir.pattern.MapSeq(mult),
                  fReduce = add,
                  init = toReg(id) $ Value(0.0f, Float)) $ tileABsram
              }),
            fReduce = add,
            init = toReg(id) $ Value(0.0f, Float)) $
          Zip(a, b))

    val dotProductRuntimeLambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, b) =>
        AccelFun(scalaDotLambdaTiled)(a, b))

    val generatedSpatial = backends.spatial.common.RuntimeCompiler(dotProductRuntimeLambda)
    println(generatedSpatial)

    val expectedOutCode =
      """|{
         |    def id_0(x: Float): Float = {
         |        x
         |    }
         |    def id_1(x: DRAM1[Float]): DRAM1[Float] = {
         |        x
         |    }
         |    def mult(l: Float, r: Float): Float = {
         |        l * r
         |    }
         |    def add(x: Float, y: Float): Float = {
         |        x + y
         |    }
         |    val v__20 = Reg[Float].buffer
         |    v__20 := id_0(0.0f)
         |    Pipe.Fold(v__20)(0 until v_N_0 by v_tileSize_1 par v_outerParFactor_2) { (v_i_11) =>
         |        val v__24 = Reg[Float].buffer
         |        v__24 := id_0(0.0f)
         |        val v__25 = SRAM[Float](v_tileSize_1)
         |        val v__26 = SRAM[Float](v_tileSize_1)
         |        Parallel {
         |            v__25 load id_1(v__16(v_i_11::(v_tileSize_1 + v_i_11) par v_innerParFactor_3))
         |            v__26 load id_1(v__17(v_i_11::(v_tileSize_1 + v_i_11) par v_innerParFactor_3))
         |        }
         |        Pipe.Fold(v__24)(0 until v_tileSize_1 by 1 par v_innerParFactor_3) { (v_i_12) =>
         |            val v__30_0 = Reg[Float]
         |            // map_seq
         |            // iteration count is exactly 1, no loop emitted
         |            val v_i_13 = Reg[Int](0)
         |            v__30_0 := mult(v__25(v_i_12), v__26(v_i_12))
         |            // end map_seq
         |            v__30_0
         |        } {
         |            add(_, _)
         |        }
         |        v__24
         |    } {
         |        add(_, _)
         |    }
         |    v__35 := id_0(v__20)
         |}""".stripMargin

    val cleanedGeneratedSpatial = cleanSpatialProgram(generatedSpatial)
    val cleanedExpectedOutCode = cleanSpatialProgram(expectedOutCode)

    assertEquals(cleanedExpectedOutCode, cleanedGeneratedSpatial)
  }

  /*
   * OpenCL examples for comparison
   */

  @Test
  def openclDotProduct(): Unit = {
    import opencl.ir._
    import opencl.ir.pattern._

    Backend.setOpenCL()

    val N = 16
    val input: Array[Float] = (0 until N).toArray.map(_.toFloat)
    val gold: Float = (input, input).zipped.map(_*_).sum

    val commonCodeOutputPath = System.getProperty("user.dir") + "/src/test/backends/spatial/host"

    val scalarDotLambda: Lambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          MapSeq(mult) $ Zip(x, y)
    )

    val codeOutputPath = s"$commonCodeOutputPath/00.OpenCLScalarDot"
    val hostCodeFileName = "opencl_scalar_dot_host.cpp"

    val hostingLambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
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
  def openclDotProductTiled(): Unit = {
    import opencl.ir._
    import opencl.ir.pattern._

    Backend.setOpenCL()

    val N = 64 //16

    val tileSize = 32 //4
    val outerParFactor = 2
    val innerParFactor = 16

    val input: Array[Float] = (0 until N).toArray.map(_.toFloat)
    val gold: Float = (input, input).zipped.map(_*_).sum

    val commonCodeOutputPath = System.getProperty("user.dir") + "/src/test/backends/spatial/host"

    val scalarDotLambdaTiled: Lambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, b) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          Join() o
          MapWrg(0)(fun(tile =>
            // Similar to the Spatial example
            //            ReduceSeq(add, 0.0f) o MapLcl(0)(mult) o
            //              toLocal(MapSeq(fun(p => Tuple(id(p._0), id(p._1))))) $ tile)) o
            // Functionally correct (there is a bug in the compiler to do with overwriting the accumulator on each
            // iteration of the outer MapSeq
            ReduceSeq(add, toGlobal(id) $ 0.0f) o MapSeq(mult) $ tile)) o
          Split(tileSize) $ Zip(a, b)
    )

    val codeOutputPath = s"$commonCodeOutputPath/01.OpenCLTiledScalarDot"
    val hostCodeFileName = "opencl_tiled_scalar_dot_host.cpp"

    val hostingLambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (x, y) =>
        ToHost() $ OclFunc(scalarDotLambdaTiled, cpu_timer = true, gpu_timer = true)(ToGPU(x), ToGPU(y))
    )

    print(opencl.executor.Compile(scalarDotLambdaTiled))
    c.global.GlobalCompiler(hostingLambda, codeOutputPath, List(hostCodeFileName))

    val actualOutput: String = backends.c.common.executor.Executor.native_compile_and_run(
      codeOutputPath, hostCodeFileName)

    print(actualOutput)

    val pattern = raw"(?:.*\n)+(\d+).*\n".r

    val pattern(count) = actualOutput.stripMargin
    assertEquals(gold, count.toFloat, 0.001f)
  }
}
