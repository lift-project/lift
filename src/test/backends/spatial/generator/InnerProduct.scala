package backends.spatial.generator

import backends.c
import backends.c.host.host_ir.{OclFunc, ToGPU, ToHost}
import backends.spatial
import backends.spatial.accel
import backends.spatial.accel.ir.pattern.{SpFold, SpForeach, SpMemFold, toDRAM, toSRAM}
import backends.spatial.host
import backends.spatial.host.ir.ast.AccelFun
import ir._
import ir.ast._
import ir.ast.debug.AssertType
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
    ArrayType(Float, N),
    ArrayType(Float, N),
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
      ArrayType(Float, N),
      ArrayType(Float, N),
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
      "arr", Seq(ArrayType(Float, tileSize)), ArrayType(Float, tileSize)) // TODO: generalise array size

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


    val scalaDotLambdaTiledHighLevel: Lambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, b) =>
        Reduce(add, Value(0.0f, Float)) o
        Map(mult) $ Zip(a, b)
    )


    val scalaDotLambdaTiled: Lambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, b) =>
        SpFold(
          iterSize = tileSize,
          stride = tileSize,
          factor = outerParFactor,

          fMap = fun(
            ArrayType(TupleType(Float, Float), tileSize), tileAB => {
              val tileA = Get(Unzip() $ tileAB, 0)
              val tileB = Get(Unzip() $ tileAB, 1)

              val tileABsram = Zip(
                /*Parallel(*/
                toSRAM(idArray) $ tileA,
                toSRAM(idArray) $ tileB
                /*)*/)

              SpFold(
                iterSize = 1,
                stride = 1,
                factor = innerParFactor,
                fMap = mult,
                fReduce = add,
                init = Value(0.0f, Float)) $ tileABsram
            }),
          fReduce = add,
          init = Value(0.0f, Float)) $
          Zip(a, b))
  }

  @Test
  def spatialGEMMTiled(): Unit = {
    val M = 128
    val P = 64
    val N = 96
    val outerFactorI = 2
    val outerFactorJ = 2
    val outerFactorK = 2
    val tileMsize = 32
    val tileNsize = 32
    val tileParFactor = 16
    val innerFactorI = 1
    val innerFactorJ = 1

    val idArray2d = UserFun("idArray", Array("arr"),
      "arr", Seq(ArrayType(ArrayType(Float, tileNsize), tileMsize)),
      ArrayType(ArrayType(Float, tileNsize), tileMsize)) // TODO: generalise array size

    val idArray1d = UserFun("idArray", Array("arr"),
      "arr", Seq(ArrayType(Float, tileMsize)), ArrayType(Float, tileMsize)) // TODO: generalise array size

    val gemmTiled: Lambda = fun(
      ArrayType(ArrayType(Float, P), M),
      ArrayType(ArrayType(Float, P), N),
      ArrayType(ArrayType(Float, N), M),
      (a, b, c) =>
        SpForeach(
          iterSize = tileMsize,
          stride = tileMsize,
          factor = outerFactorI,
          f = fun(
          ArrayType(TupleType(ArrayType(Float, P), ArrayType(Float, N)), tileMsize),
          tileACrows => {
            val tileArows =
              AssertType(ArrayType(ArrayType(Float, tileMsize), P), "tileArows.type") o
                Transpose() $ Get(Unzip() $ tileACrows, 0)
            val tileCrows =
              AssertType(ArrayType(ArrayType(Float, tileMsize), N), "tileCrows.type") o
                Transpose() $ Get(Unzip() $ tileACrows, 1)

            SpForeach(
              iterSize = tileNsize,
              stride = tileNsize,
              factor = outerFactorJ,
              f = fun(
              ArrayType(TupleType(ArrayType(Float, P), ArrayType(Float, tileMsize)), tileNsize),
              tileBcolsC => {

                val tileBcols =
                  AssertType(ArrayType(ArrayType(Float, tileNsize), P), "tileBcols.type") o
                    Transpose() $ Get(Unzip() $ tileBcolsC, 0)
                val tileCsram =
                  AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "tileCsram.type") o
                    toSRAM(idArray2d) o Transpose() $ Get(Unzip() $ tileBcolsC, 1)

                SpMemFold(
                  iterSize = tileMsize,
                  stride = tileMsize,
                  factor = outerFactorK,
                  fMap = fun(
                    ArrayType(TupleType(ArrayType(Float, tileNsize), ArrayType(Float, tileNsize)), tileMsize),
                    tileAB => {
                      val tileA = AssertType(ArrayType(ArrayType(Float, tileMsize), tileMsize), "tileA") $
                        Get(Unzip() $ tileAB, 0)
                      val tileBsram = AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "tileBsram") o
                        toSRAM(idArray2d) $ Get(Unzip() $ tileAB, 1)

                      SpForeach(
                        iterSize = 1,
                        stride = 1,
                        factor = innerFactorI,
                        f = fun(
                          // TODO: get rid of the extra dimension of 1 element
                          ArrayType(TupleType(ArrayType(Float, tileMsize), ArrayType(Float, tileNsize)), 1),
                          tileRowABsram => {
                            val tileRowAsram = AssertType(ArrayType(Float, tileMsize), "tileRowAsram") o
                              toSRAM(idArray1d) o Join() $ Get(Unzip() $ tileRowABsram, 0)
                            val tileRowBsram = AssertType(ArrayType(Float, tileNsize), "tileRowBsram") o
                              Join() $ Get(Unzip() $ tileRowABsram, 1)

                            SpForeach(
                              iterSize = 1,
                              stride = 1,
                              factor = innerFactorJ,
                              f = fun(Float, elBsram =>
                                /*Pipe {*/
                                SpMemFold(
                                  iterSize = 1,
                                  stride = 1,
                                  factor = tileParFactor,
                                  fMap = fun(Float, elAsram =>
                                    mult(elBsram, elAsram)
                                  ),
                                  fReduce = add,
                                  init = Value(0.0f, Float)
                                ) $ tileRowAsram
                                /*}*/
                              )) $ tileRowBsram
                          })) $ Zip(tileA, tileBsram)
                    }),
                  fReduce = add,
                  init = tileCsram
                ) $ Zip(tileArows, tileBcols)
              })) $ Zip(b, tileCrows)
          })) $ Zip(a, c)
    )

    val expectedOutCode = """  
      Accel {

        Foreach(M by tileMsize par outerFactorI,
                N by tileNsize par outerFactorJ) { (i, j) =>

          val tileC = SRAM[T](tileMsize, tileNsize).buffer
          tileC load c_dram(i::i+tileMsize, j::j+tileNsize par tileParFactor)

          MemFold(tileC par tileParFactor)(P by tileMsize par outerFactorK) { k =>

            val tileCaccum = SRAM[T](tileMsize, tileNsize)

            val bSRAM      = SRAM[T](tileNsize, tileNsize)
            bSRAM load b_dram(k::k+tileMsize, j::j+tileMsize par tileParFactor)

            Foreach(tileMsize by 1 par innerFactorI) { ii =>

              val aSRAM = SRAM[T](tileMsize)

              aSRAM load a_dram(i+ii, k::k+tileMsize par tileParFactor)

              Foreach(tileNsize by 1 par innerFactorJ) { jj =>

                Pipe {
                  tileCaccum(ii,jj) = Reduce(Reg[T])(tileMsize by 1 par tileParFactor) { kk =>
                    bSRAM(kk, jj) * aSRAM(kk)
                  }{_+_}
                }
              }
            }
            tileCaccum
          }{_+_}

          cDRAM(i::i+tileNsize, j::j+tileMsize par tileParFactor) store tileC
        }
      }
    """
  }
}
