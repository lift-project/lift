package backends.spatial.generator

import backends.c
import backends.c.host.host_ir.{OclFunc, ToGPU, ToHost}
import backends.spatial
import backends.spatial.accel
import backends.spatial.accel.ir.pattern.{MapPar, toDRAM, toSRAM}
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

  @Test
  def spatialDotProductTiled(): Unit = {

    val tileSize = 32
    val outerParFactor = 2
    val innerParFactor = 16

    val idArray = UserFun("idArray", Array("arr"),
      "arr", Seq(ArrayTypeWSWC(Float, tileSize)), ArrayTypeWSWC(Float, tileSize)) // TODO: generalise array size

    val loadAndReduceTile: Lambda = fun(
      TupleType(
        ArrayTypeWSWC(Float, tileSize),
        ArrayTypeWSWC(Float, tileSize)),
      tile =>
        /* Reduce a single tile */
        accel.ir.pattern.ReducePar(
          f = fun((acc, aElement, bElement) => add(acc, mult(aElement, bElement))),
          init = 0.0f,
          p = innerParFactor) o

          fun((aTile, bTile) => Zip(
            /*Parallel(*/
            toSRAM(idArray) $ aTile,
            toSRAM(idArray) $ bTile
            /*)*/)) o
          Unzip() $ tile)

    val scalarDotLambdaTiled: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (x, y) =>
        toDRAM(accel.ir.pattern.MapSeq(id)) o
          accel.ir.pattern.ReduceParStride(
            f = fun((acc, tile) => add(acc, loadAndReduceTile(tile))),
            init = 0.0f, s = tileSize, p = outerParFactor) $
          Zip(x, y))

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

  @Test
  def spatialGEMMTiled(): Unit = {
    val matSize = 64
    val outerFactor_i = 2
    val outerFactor_j = 2
    val outerTileW = 32
    val outerTileH = 32
    val innerTileWH = 16
    val innerFactorI = 1
    val innerFactorJ = 1


    val expectedOutCode = """  
      Accel {

        Foreach(matSize by outerTileH par outerFactor_i,
                matSize by outerTileW par outerFactor_j) { (i, j) =>

          val tileC = SRAM[T](outerTileH, outerTileW)

          MemReduce(tileC par innerTileWH)(matSize by outerTileW par loop_kk) { kk =>

            val tileCAccum = SRAM[T](outerTileH, outerTileW)

            val bSRAM      = SRAM[T](outerTileW, outerTileW)
            bSRAM load b_dram(kk::kk+outerTileW, j::j+outerTileW par innerTileWH)

            Foreach(outerTileH by 1 par innerFactorI) { ii =>

              val aSRAM = SRAM[T](outerTileW)

              aSRAM load a_dram(i+ii, kk::kk+outerTileW par innerTileWH)

              Foreach(outerTileW by 1 par innerFactorJ) { jj =>

                Pipe {
                  tileCaccum(ii,jj) = Reduce(Reg[T])(outerTileW by 1 par innerTileWH) { k =>
                    bSRAM(k, jj) * aSRAM(k)
                  }{_+_}
                }
              }
            }
            tileCAccum
          }{_+_}

          cDRAM(i::i+outerTileH, j::j+outerTileW par innerTileWH) store tileC
        }
      }
    """
  }
}
