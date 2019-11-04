package backends.spatial.generator

import backends.c.host.host_ir.{OclFunc, ToGPU, ToHost}
import backends.spatial.accel.ir.pattern.toReg
import backends.{Backend, c, spatial}
import ir._
import ir.ast._
import ir.ast.debug.{AssertType, PrintType}
import lift.arithmetic.SizeVar
import opencl.executor.TestWithExecutor
import org.junit.Assert._
import org.junit.{Ignore, Test}

import scala.util.matching.Regex

object InnerProduct extends TestWithExecutor

class InnerProduct {

  @Test
  def openclDotProduct(): Unit = {
    import opencl.ir._
    import opencl.ir.pattern._

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
  def openclDotProductTiled(): Unit = {
    import opencl.ir._
    import opencl.ir.pattern._

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
          MapSeq(fun(tile =>
            // Similar to the Spatial example
//            ReduceSeq(add, /*toGlobal(id) $ */0.0f) o MapSeq(mult) o
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

  /**
   * Cleans the Spatial program by replacing the numeric in variable names with letters
   * in increasing alphabetic order. The letters are assigned to variables in their order
   * of occurrence in the program, thus replacing the ordering of the original numeric ids, which
   * is affected by the internal mechanics of the compiler. Example:
   * { val v_someName_37 = 1
   *   v_someName_37 + v__2
   * } is replaced with
   * { val v_someName_b = 1
   *   v_someName_b + v__a
   * }
   */
  def cleanSpatialProgram(code: String): String = {
    val regexVarNames = new Regex("""(v_(.*?)_[0-9]+(__[0-9]+(_[0-9]+)*)*)""", "varFullName", "varCoreName")
    val varSubstitutions = regexVarNames.
      findAllMatchIn(code).
      map(m => (m.group("varFullName"), m.group("varCoreName"))).
      toList.distinct.
      zipWithIndex.map {
        case ((varFullName: String, varCoreName: String), orderId: Int) =>
          if (!varCoreName.isEmpty) (varFullName, "v_" + varCoreName + "_"  + (97+orderId).toChar)
          else (varFullName, "v__" + (97+orderId).toChar)
      }
    val updatedCode = varSubstitutions.foldLeft(code) {
      case (partUpdatedCode, varSubst) => partUpdatedCode.replaceAllLiterally(varSubst._1, varSubst._2)
    }
    updatedCode
  }

  @Test
  def spatialDotProduct(): Unit = {
    import backends.spatial.common.ir._
    import backends.spatial.host

    val N = 1024
    val input: Array[Float] = (0 until 16).toArray.map(_.toFloat)
    val gold: Float = (input, input).zipped.map(_*_).sum

    val commonCodeOutputPath = System.getProperty("user.dir") + "/src/test/backends.spatial/host"

    val scalarDotLambda: Lambda = null
//      fun(
//      ArrayType(Float, N),
//      ArrayType(Float, N),
//      (x, y) =>
//        toGlobal(MapSeq(id)) o
//          ReduceSeq(add, 0.0f) o
//          MapSeq(mult) $ Zip(x, y)
//    )

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

    val out = spatial.common.RuntimeCompiler(runTimeLambda)

  }

  @Test
  def spatialDotProductTiled(): Unit = {
    import backends.spatial.accel.ir._
    import backends.spatial.accel.ir.pattern.{SpFold, toSRAM}
    import backends.spatial.common.ir._
    import backends.spatial.host.ir.ast.AccelFun

    Backend.setSpatial()

    val N = SizeVar("N") //1024
    val tileSize = SizeVar("tileSize") //32
    val outerParFactor = SizeVar("outerParFactor") //2
    val innerParFactor = SizeVar("innerParFactor") //16
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
//    """

    val id = UserFun("id", Array("x"), "x", Seq(Float), Float)

    val idArray = UserFun("idArray", Array("arr"),
      "arr", Seq(ArrayType(Float, tileSize)), ArrayType(Float, tileSize)) // TODO: generalise array size


    val scalaDotLambdaTiled: Lambda = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, b) =>
        SpFold(chunkSize = tileSize, stride = tileSize, factor = outerParFactor,
          fMap = fun(

            ArrayType(TupleType(Float, Float), tileSize), tileAB => {
              val tileA = Get(Unzip() $ tileAB, 0)
              val tileB = Get(Unzip() $ tileAB, 1)

              val tileABsram = Zip(
                /*Parallel(*/
                toSRAM(idArray) $ tileA,
                toSRAM(idArray) $ tileB
                /*)*/)

              SpFold(chunkSize = 1, stride = 1, factor = innerParFactor,
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
         |    def idArray(arr: DRAM1[Float]): DRAM1[Float] = {
         |        arr
         |    }
         |    def mult(l: Float, r: Float): Float = {
         |        l * r
         |    }
         |    def add(x: Float, y: Float): Float = {
         |        x + y
         |    }
         |    val v__18 = Reg[Float](0.0f)
         |    Fold(v__18)(0 until v_N_0 by v_tileSize_1 par v_outerParFactor_2) { (v_i_10) =>
         |        val v__21 = Reg[Float](0.0f)
         |        val v__22 = SRAM[Float](v_tileSize_1)
         |        v__22 load idArray(v__15(v_i_10::(v_tileSize_1 + v_i_10)))
         |        val v__23 = SRAM[Float](v_tileSize_1)
         |        v__23 load idArray(v__16(v_i_10::(v_tileSize_1 + v_i_10)))
         |        Fold(v__21)(0 until v_tileSize_1 by 1 par v_innerParFactor_3) { (v_i_11) =>
         |            val v__27_0 = Reg[Float]
         |            // map_seq
         |            // iteration count is exactly 1, no loop emitted
         |            val v_i_12 = Reg[Int](0)
         |            v__27_0 := mult(v__22(v_i_11), v__23(v_i_11))
         |            // end map_seq
         |            v__27_0
         |        } {
         |            add(_, _)
         |        }
         |    } {
         |        add(_, _)
         |    }
         |}""".stripMargin

    val cleanedGeneratedSpatial = cleanSpatialProgram(generatedSpatial)
    val cleanedExpectedOutCode = cleanSpatialProgram(expectedOutCode)

    assertEquals(cleanedExpectedOutCode, cleanedGeneratedSpatial)
  }

  @Test
  def spatialGEMMTiled(): Unit = {
    import backends.spatial.accel.ir.pattern.{SpForeach, SpMemFold, toDRAM, toSRAM}
    import backends.spatial.common.ir._
    import backends.spatial.accel.ir._
    import backends.spatial.host
    import backends.spatial.accel.ir.pattern.SpFold
    import backends.spatial.accel.ir.pattern.MapSeq

    Backend.setSpatial()

    val M = SizeVar("M") // 128
    val P = SizeVar("P") // 64
    val N = SizeVar("N") // 96
    val outerFactorI = SizeVar("outerFactorI") // 2
    val outerFactorJ = SizeVar("outerFactorJ") // 2
    val outerFactorK = SizeVar("outerFactorK") // 2
    val tileMsize = SizeVar("tileMsize") // 32
    val tileNsize = SizeVar("tileNsize") // 16
    val tileParFactor = SizeVar("tileParFactor") // 16
    val innerFactorI = SizeVar("innerFactorI") // 1
    val innerFactorJ = SizeVar("innerFactorJ") // 1

    val id = UserFun("id", Array("x"), "x", Seq(Float), Float)

    val idArray2dMN = UserFun("idArray2dMN", Array("arr"),
      "arr", Seq(ArrayType(ArrayType(Float, tileNsize), tileMsize)),
      ArrayType(ArrayType(Float, tileNsize), tileMsize)) // TODO: generalise array size

    val idArray2dNN = UserFun("idArray2dNN", Array("arr"),
      "arr", Seq(ArrayType(ArrayType(Float, tileNsize), tileNsize)),
      ArrayType(ArrayType(Float, tileNsize), tileNsize)) // TODO: generalise array size

    val idArray1dM = UserFun("idArray1dM", Array("arr"),
      "arr", Seq(ArrayType(Float, tileMsize)), ArrayType(Float, tileMsize)) // TODO: generalise array size

    val idArray1dN = UserFun("idArray1dN", Array("arr"),
      "arr", Seq(ArrayType(Float, tileNsize)), ArrayType(Float, tileNsize)) // TODO: generalise array size

    val addMatrices = UserFun("addMatrices", Array("l", "r"),
      "addMatrices", Seq(ArrayType(ArrayType(Float, tileNsize), tileMsize),
        ArrayType(ArrayType(Float, tileNsize), tileMsize)),
      ArrayType(ArrayType(Float, tileNsize), tileMsize)) // TODO: generalise array size

    val gemmTiled: Lambda = fun(
      ArrayType(ArrayType(Float, P), M),
      ArrayType(ArrayType(Float, P), N),
      ArrayType(ArrayType(Float, N), M),
      (a, b, c) =>
        SpForeach(chunkSize = tileMsize, stride = tileMsize, factor = outerFactorI,
          f = fun(
            ArrayType(TupleType(ArrayType(Float, P), ArrayType(Float, N)), tileMsize),
            tileACrows => {

              val tileArows =
                AssertType(ArrayType(ArrayType(Float, tileMsize), P), "tileArows.type") o
                  Transpose() $ Get(Unzip() $ tileACrows, 0)
              val tileCrows =
                AssertType(ArrayType(ArrayType(Float, tileMsize), N), "tileCrows.type") o
                  Transpose() $ Get(Unzip() $ tileACrows, 1)

              SpForeach(chunkSize = tileNsize, stride = tileNsize, factor = outerFactorJ,
                f = fun(
                  ArrayType(TupleType(ArrayType(Float, P), ArrayType(Float, tileMsize)), tileNsize),
                  tileBcolsC => {

                    val tileBcols =
                      AssertType(ArrayType(ArrayType(Float, tileNsize), P), "tileBcols.type") o
                        Transpose() $ Get(Unzip() $ tileBcolsC, 0)
                    val tileCsram =
                      AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "tileCsram.type") o
                        toSRAM(idArray2dMN) o Transpose() $ Get(Unzip() $ tileBcolsC, 1)

                    toDRAM(
                      SpMemFold(chunkSize = tileNsize, stride = tileNsize, factor = outerFactorK,
                        fMap = fun(
                          ArrayType(TupleType(ArrayType(Float, tileMsize), ArrayType(Float, tileNsize)), tileNsize),
                          tileAB => {
                            // TODO: confirm whether Transpose should be used instead of TransposeW below
                            val tileA = AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "tileA") o
                              Transpose() $ Get(Unzip() $ tileAB, 0)
                            val tileBsram = AssertType(ArrayType(ArrayType(Float, tileNsize), tileNsize), "tileBsram") o
                              Transpose() o toSRAM(idArray2dNN) $ Get(Unzip() $ tileAB, 1)

                            AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "Outer MemFold fMap type") o
                              // The Let below causes tileBsram to materialise (declare mem and issue the load statement)
                              // outside of the next SpForeach instead of inside (right before it is to be read)
                              Let(tileBsramMaterialised =>
                                SpForeach(chunkSize = 1, stride = 1, factor = innerFactorI,
                                  f = fun(
                                    ArrayType(ArrayType(Float, tileNsize), 1),
                                    tileRowA => {
                                      val tileRowAsram = AssertType(ArrayType(Float, tileNsize), "tileRowAsram") o
                                        toSRAM(idArray1dN) o Join() $ tileRowA

                                      Let(tileRowAsramMaterialised =>
                                        Join() o
                                          SpForeach(
                                            chunkSize = 1,
                                            stride = 1,
                                            factor = innerFactorJ,
                                            f = fun(ArrayType(ArrayType(Float, tileNsize), 1), tileRowBsram =>

                                              AssertType(ArrayType(Float, 1), "Inner MemFold result type") o
                                                //
                                                /*Pipe {*/
                                                SpFold(chunkSize = 1, stride = 1, factor = tileParFactor,
                                                  fMap = fun(
                                                    ArrayType(TupleType(Float, Float), 1), elAsramBsram =>
                                                      MapSeq(mult) $ elAsramBsram),
                                                  fReduce = add,
                                                  init = toReg(id) $ Value(0.0f, Float)
                                                ) $ Zip(tileRowAsramMaterialised, Join() $ tileRowBsram)
                                              /*}*/
                                            )) $ tileBsramMaterialised
                                      ) $ tileRowAsram
                                    })) $ tileA
                              ) $ tileBsram
                          }),
                        fReduce = /*addMatrices*/ add, init = tileCsram
                      )) $ Zip(tileArows, tileBcols)
                  })) $ Zip(b, tileCrows)
            })) $ Zip(a, c))

    val runTimeLambda: Lambda = fun(
      ArrayType(ArrayType(Float, P), M),
      ArrayType(ArrayType(Float, P), N),
      ArrayType(ArrayType(Float, N), M),
      (a, b, c) =>
        host.ir.ast.AccelFun(gemmTiled)(a, b, c)
    )

    val out = spatial.common.RuntimeCompiler(runTimeLambda)

    val expectedOutCode = """  
      Accel {

        Foreach(M by tileMsize par outerFactorI,
                N by tileNsize par outerFactorJ) { (i, j) =>

          val tileC = SRAM[T](tileMsize, tileNsize).buffer
          tileC load c_dram(i::i+tileMsize, j::j+tileNsize par tileParFactor)

          MemFold(tileC par tileParFactor)(P by tileNsize par outerFactorK) { k =>

            val tileCaccum = SRAM[T](tileMsize, tileNsize)

            val bSRAM      = SRAM[T](tileNsize, tileNsize)
            bSRAM load b_dram(k::k+tileNsize, j::j+tileNsize par tileParFactor)

            Foreach(tileMsize by 1 par innerFactorI) { ii =>

              val aSRAM = SRAM[T](tileNsize)

              aSRAM load a_dram(i+ii, k::k+tileNsize par tileParFactor)

              Foreach(tileNsize by 1 par innerFactorJ) { jj =>

                Pipe {
                  tileCaccum(ii,jj) = Reduce(Reg[T])(tileNsize by 1 par tileParFactor) { kk =>
                    bSRAM(kk, jj) * aSRAM(kk)
                  }{_+_}
                }
              }
            }
            tileCaccum
          }{_+_}

          cDRAM(i::i+tileMsize, j::j+tileNsize par tileParFactor) store tileC
        }
      }
    """

    println(out)
  }
}
