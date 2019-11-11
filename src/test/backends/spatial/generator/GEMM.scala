package backends.spatial.generator

import arithmetic.TypeVar
import backends.{Backend, spatial}
import ir._
import ir.ast._
import ir.ast.debug.{AssertType, PrintType}
import lift.arithmetic.SizeVar
import org.junit.Assert._
import org.junit.{AfterClass, Test}
import backends.spatial.accel.ir._
import backends.spatial.accel.ir.pattern._
import backends.spatial.common.ir._
import backends.spatial.host


object GEMM {
  @AfterClass def after(): Unit = {
    Backend.setOpenCL()
  }
}

class GEMM {
  @Test
  def spatialGEMMTiled(): Unit = {

    Backend.setSpatial()

    //    The Spatial code we are looking to generate ideally:
//    Accel {
//
//      Foreach(M by tileMsize par outerFactorI,
//              N by tileNsize par outerFactorJ) { (i, j) =>
//
//        val tileC = SRAM[T](tileMsize, tileNsize).buffer
//        tileC load c_dram(i::i+tileMsize, j::j+tileNsize par tileParFactor)
//
//        MemFold(tileC par tileParFactor)(P by tileNsize par outerFactorK) { k =>
//
//          val tileCaccum = SRAM[T](tileMsize, tileNsize)
//
//          val bSRAM      = SRAM[T](tileNsize, tileNsize)
//          bSRAM load b_dram(j::j+tileNsize, k::k+tileNsize par tileParFactor)
//
//          Foreach(tileMsize by 1 par innerFactorI) { ii =>
//
//            val aSRAM = SRAM[T](tileNsize)
//
//            aSRAM load a_dram(i+ii, k::k+tileNsize par tileParFactor)
//
//            Foreach(tileNsize by 1 par innerFactorJ) { jj =>
//
//              tileCaccum(ii,jj) = Reduce(Reg[T])(tileNsize by 1 par tileParFactor) { kk =>
//                bSRAM(kk, jj) * aSRAM(kk)
//              }{_+_}
//            }
//          }
//          tileCaccum
//        }{_+_}
//
//        cDRAM(i::i+tileMsize, j::j+tileNsize par tileParFactor) store tileC
//      }
//    }
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

    val x = TypeVar()
    val y = TypeVar()

    val id = UserFun("id", Array("x"), "x", Seq(Float), Float)
    val id1D = UserFun("id", Array("x"), "x", Seq(ArrayType(Float, x)), ArrayType(Float, x))
    val id2D = UserFun("id", Array("x"), "x", Seq(ArrayType(ArrayType(Float, x), y)), ArrayType(ArrayType(Float, x), y))

    val gemmTiled: Lambda = fun(
      ArrayType(ArrayType(Float, P), M),
      ArrayType(ArrayType(Float, N), P),
      ArrayType(ArrayType(Float, N), M),
      (a, b, c) =>
        PrintType() o
          Join() o
          SpPipeForeach(chunkSize = tileMsize, stride = tileMsize, factor = outerFactorI,
            f = fun(
              ArrayType(TupleType(ArrayType(Float, P), ArrayType(Float, N)), tileMsize),
              tileACrows => {

                val tileArows =
                  AssertType(ArrayType(ArrayType(Float, tileMsize), P), "tileArows.type") o
                    Transpose() $ Get(Unzip() $ tileACrows, 0)
                val tileCrows =
                  AssertType(ArrayType(ArrayType(Float, tileMsize), N), "tileCrows.type") o
                    Transpose() $ Get(Unzip() $ tileACrows, 1)

                Map(Join()) o TransposeW() o PrintType() o
                  SpPipeForeach(chunkSize = tileNsize, stride = tileNsize, factor = outerFactorJ,
                    f = fun(
                      ArrayType(TupleType(ArrayType(Float, P), ArrayType(Float, tileMsize)), tileNsize),
                      tileBcolsC => {

                        val tileBcols =
                          AssertType(ArrayType(ArrayType(Float, tileNsize), P), "tileBcols.type") o
                            Transpose() $ Get(Unzip() $ tileBcolsC, 0)
                        val tileCsram =
                          AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "tileCsram.type") o
                            toSRAM(BurstUserFun(id2D, tileParFactor)) o Transpose() $ Get(Unzip() $ tileBcolsC, 1)

                        toDRAM(BurstUserFun(id2D, tileParFactor)) o PrintType() o
                          SpPipeMemFold(chunkSize = tileNsize, stride = tileNsize, factor = outerFactorK,
                            fMap = fun(
                              ArrayType(TupleType(ArrayType(Float, tileMsize), ArrayType(Float, tileNsize)), tileNsize),
                              tileAB => {
                                val tileA = AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "tileA") o
                                  Transpose() $ Get(Unzip() $ tileAB, 0)
                                // TODO: confirm whether Transpose should be used instead of TransposeW below
                                val tileBsram = AssertType(ArrayType(ArrayType(Float, tileNsize), tileNsize), "tileBsram") o
                                  Transpose() o toSRAM(BurstUserFun(id2D, tileParFactor)) $ Get(Unzip() $ tileAB, 1)

                                AssertType(ArrayType(ArrayType(Float, tileNsize), tileMsize), "Outer MemFold fMap type") o
                                  // The Let below causes tileBsram to materialise (declare mem and issue the load statement)
                                  // outside of the next SpForeach instead of inside (right before it is to be read)
                                  Let(tileBsramMaterialised =>
                                    SpPipeForeach(chunkSize = 1, stride = 1, factor = innerFactorI,
                                      f = fun(
                                        ArrayType(ArrayType(Float, tileNsize), 1),
                                        tileRowA => {
                                          val tileRowAsram = AssertType(ArrayType(Float, tileNsize), "tileRowAsram") o
                                            toSRAM(BurstUserFun(id1D, tileParFactor)) o Join() $ tileRowA

                                          Let(tileRowAsramMaterialised =>
                                            SpPipeForeach(
                                              chunkSize = 1,
                                              stride = 1,
                                              factor = innerFactorJ,
                                              f = fun(ArrayType(ArrayType(Float, tileNsize), 1), tileRowBsram =>

                                                AssertType(Float, "Inner MemFold result type") o
                                                  toSRAM(id) o
                                                  //
                                                  SpPipeFold(chunkSize = 1, stride = 1, factor = tileParFactor,
                                                    fMap = fun(
                                                      ArrayType(TupleType(Float, Float), 1), elAsramBsram =>
                                                        MapSeq(mult) $ elAsramBsram),
                                                    fReduce = add,
                                                    init = toReg(id) $ Value(0.0f, Float)
                                                  ) $ Zip(tileRowAsramMaterialised, Join() $ tileRowBsram)
                                              )) $ tileBsramMaterialised
                                          ) $ tileRowAsram
                                        })) $ tileA
                                  ) $ tileBsram
                              }),
                            fReduce = /*addMatrices*/ add, init = tileCsram
                          ) $ Zip(tileArows, tileBcols)
                      })) $ Zip(Transpose() $ b, tileCrows)
              })) $ Zip(a, c))

    val runTimeLambda: Lambda = fun(
      ArrayType(ArrayType(Float, P), M),
      ArrayType(ArrayType(Float, P), N),
      ArrayType(ArrayType(Float, N), M),
      (a, b, c) =>
        host.ir.ast.AccelFun(gemmTiled)(a, b, c)
    )

    val generatedSpatial = spatial.common.RuntimeCompiler(runTimeLambda)
    println(generatedSpatial)

    val expectedOutCode =
      """|{
         |    def id_3(x: SRAM2[Float]): SRAM2[Float] = {
         |        x
         |    }
         |    def id_2(x: Float): Float = {
         |        x
         |    }
         |    def id_1(x: DRAM1[Float]): DRAM1[Float] = {
         |        x
         |    }
         |    def id_0(x: DRAM2[Float]): DRAM2[Float] = {
         |        x
         |    }
         |    def mult(l: Float, r: Float): Float = {
         |        l * r
         |    }
         |    def add(x: Float, y: Float): Float = {
         |        x + y
         |    }
         |    // SpForeach
         |    Pipe.Foreach(0 until v_M_0 by v_tileMsize_6 par v_outerFactorI_3) { (v_i_24) =>
         |        // SpForeach
         |        Pipe.Foreach(0 until v_N_2 by v_tileNsize_7 par v_outerFactorJ_4) { (v_i_25) =>
         |            val v__38 = SRAM[Float](v_tileMsize_6, v_tileNsize_7).buffer
         |            v__38 load id_0(v__35(v_i_24::(v_tileMsize_6 + v_i_24), v_i_25::(v_tileNsize_7 + v_i_25) par v_tileParFactor_8))
         |            Pipe.MemFold(v__38)(0 until v_P_1 by v_tileNsize_7 par v_outerFactorK_5) { (v_i_26) =>
         |                val v__41 = SRAM[Float](v_tileNsize_7, v_tileNsize_7)
         |                v__41 load id_0(v__34(v_i_26::(v_tileNsize_7 + v_i_26), v_i_25::(v_tileNsize_7 + v_i_25) par v_tileParFactor_8))
         |                val v__52 = SRAM[Float](v_tileMsize_6, v_tileNsize_7)
         |                // SpForeach
         |                Pipe.Foreach(0 until v_tileMsize_6 by 1 par v_innerFactorI_9) { (v_i_27) =>
         |                    val v__42 = SRAM[Float](v_tileNsize_7)
         |                    v__42 load id_1(v__33((v_i_24 + v_i_27), v_i_26::(v_tileNsize_7 + v_i_26) par v_tileParFactor_8))
         |                    // SpForeach
         |                    Pipe.Foreach(0 until v_tileNsize_7 by 1 par v_innerFactorJ_10) { (v_i_28) =>
         |                        val v__45 = Reg[Float].buffer
         |                        v__45 := id_2(0.0f)
         |                        Pipe.Fold(v__45)(0 until v_tileNsize_7 by 1 par v_tileParFactor_8) { (v_i_29) =>
         |                            val v__49_0 = Reg[Float]
         |                            // map_seq
         |                            // iteration count is exactly 1, no loop emitted
         |                            val v_i_30 = Reg[Int](0)
         |                            v__49_0 := mult(v__42(v_i_29), v__41(v_i_29, v_i_28))
         |                            // end map_seq
         |                            v__49_0
         |                        } {
         |                            add(_, _)
         |                        }
         |                        v__52(v_i_27, v_i_28) = id_2(v__45)
         |                    }
         |                    // end SpForeach
         |                }
         |                // end SpForeach
         |                v__52(0::v_tileMsize_6, 0::v_tileNsize_7)
         |            } {
         |                add(_, _)
         |            }
         |            v__55(v_i_24::(v_tileMsize_6 + v_i_24), v_i_25::(v_tileNsize_7 + v_i_25) par v_tileParFactor_8) store id_3(v__38(0::v_tileMsize_6, 0::v_tileNsize_7))
         |        }
         |        // end SpForeach
         |    }
         |    // end SpForeach
         |}""".stripMargin

    val cleanedGeneratedSpatial = cleanSpatialProgram(generatedSpatial)
    val cleanedExpectedOutCode = cleanSpatialProgram(expectedOutCode)

    assertEquals(cleanedExpectedOutCode, cleanedGeneratedSpatial)
  }
}
