package backends.spatial.generator

import arithmetic.TypeVar
import backends.spatial.accel.ir._
import backends.spatial.accel.ir.pattern._
import backends.spatial.common.ir._
import backends.spatial.host
import backends.{Backend, spatial}
import ir.ast.debug.{AssertType, PrintType}
import ir.ast.{Drop, Get, Lambda, Let, SkipW, Split, Tuple, Unzip, UserFun, Value, Zip, fun}
import ir.{ArrayType, TupleType}
import lift.arithmetic.{NewFactorizationOfSum, SizeVar}
import org.junit.Assert.assertEquals
import org.junit.{AfterClass, Test}

object LSTM {
  @AfterClass def after(): Unit = {
    Backend.setOpenCL()
  }
}

class LSTM {

  @Test
  def lstm(): Unit = {
    val originalNewFactorizationEnabledStatus = NewFactorizationOfSum()
    NewFactorizationOfSum.enabled = true

    Backend.setSpatial()

    val nSteps = SizeVar("nSteps")
    val h = SizeVar("h") // nCells
    val d = SizeVar("d") // nFeatures
    val hu = SizeVar("hu")
    val rv = SizeVar("rv")
    val ru = SizeVar("ru")
    val nLutValues = SizeVar("nLutValues")

    val a = TypeVar()
    val b = TypeVar()

    val id = UserFun("id", Array("x"), "x", Seq(Float), Float)
    val id1D = UserFun("id", Array("x"), "x", Seq(ArrayType(Float, a)), ArrayType(Float, a))
    val id2D = UserFun("id", Array("x"), "x", Seq(ArrayType(ArrayType(Float, a), b)), ArrayType(ArrayType(Float, a), b))
    val lookup = UserFun("lookup", Array("x", "lut"),
      "lut(((x + lutBound) / lutResolution).to[Int])", Seq(Float, ArrayType(Float, a)), Float)

    def layerLambda: Lambda = fun(
      /* xh:      */ ArrayType(Float, nSteps * (h + d) + h),
      /* c:       */ ArrayType(Float, h),
      /* wI:      */ ArrayType(ArrayType(Float, h + d), h),
      /* wG:      */ ArrayType(ArrayType(Float, h + d), h),
      /* wF:      */ ArrayType(ArrayType(Float, h + d), h),
      /* wO:      */ ArrayType(ArrayType(Float, h + d), h),
      /* bI:      */ ArrayType(Float, h),
      /* bG:      */ ArrayType(Float, h),
      /* bF:      */ ArrayType(Float, h),
      /* bO:      */ ArrayType(Float, h),
      /* lutI:    */ ArrayType(Float, nLutValues),
      /* lutG:    */ ArrayType(Float, nLutValues),
      /* lutF:    */ ArrayType(Float, nLutValues),
      /* lutO:    */ ArrayType(Float, nLutValues),
      /* lutTanh: */ ArrayType(Float, nLutValues),
      (hx, c,
       wI, wG, wF, wO,
       bI, bG, bF, bO,
       lutI, lutG, lutF, lutO, lutTanh) =>

        Value("0.0f", ArrayType(Float, 1)) :>>
        ReduceSeq(
          // Write back to c and to h sectors of hx in DRAM
          init = Tuple(c, hx),
          f = fun((_, _) => {

            c :>> toSRAM(id1D) :>> Let(cSRAM => {
            wI :>> toSRAM(id2D) :>> Let(wISRAM => {
            wG :>> toSRAM(id2D) :>> Let(wGSRAM => {
            wF :>> toSRAM(id2D) :>> Let(wFSRAM => {
            wO :>> toSRAM(id2D) :>> Let(wOSRAM => {
            bI :>> toSRAM(id1D) :>> Let(bISRAM => {
            bG :>> toSRAM(id1D) :>> Let(bGSRAM => {
            bF :>> toSRAM(id1D) :>> Let(bFSRAM => {
            bO :>> toSRAM(id1D) :>> Let(bOSRAM => {
            lutI :>> toSRAM(id1D) :>> Let(lutISRAM => {
            lutG :>> toSRAM(id1D) :>> Let(lutGSRAM => {
            lutF :>> toSRAM(id1D) :>> Let(lutFSRAM => {
            lutO :>> toSRAM(id1D) :>> Let(lutOSRAM => {
            lutTanh :>> toSRAM(id1D) :>> Let(lutTanhSRAM => {

              hx :>> Drop(left = 0, right = h) :>> Split(h+d) :>>
              MapAccumSeq(
                init = cSRAM,

                f = fun(
                  /* c of the previous step */
                  ArrayType(Float, h),
                  /* h of the previous step and x of the current step: */
                  ArrayType(Float, h+d),
                  (cPrev, hPrevXCur) => {

                    hPrevXCur :>> toSRAM(id1D) :>> Let(hPrevXCurSRAM => {

                      Zip(cPrev, wISRAM, wGSRAM, wFSRAM, wOSRAM, bISRAM, bGSRAM, bFSRAM, bOSRAM) :>>
                        //
                        SpPipeForeach(chunkSize = 1, stride = 1, factor = hu,
                          f = fun(netParams => {
                            val cellC = AssertType(Float, "cellC") $ Get(netParams.at(0), 0)
                            val cellWI = AssertType(ArrayType(Float, d + h), "cellWI") $ Get(netParams.at(0), 1)
                            val cellWG = AssertType(ArrayType(Float, d + h), "cellWG") $ Get(netParams.at(0), 2)
                            val cellWF = AssertType(ArrayType(Float, d + h), "cellWF") $ Get(netParams.at(0), 3)
                            val cellWO = AssertType(ArrayType(Float, d + h), "cellWO") $ Get(netParams.at(0), 4)
                            val cellBI = AssertType(Float, "cellBI") $ Get(netParams.at(0), 5)
                            val cellBG = AssertType(Float, "cellBG") $ Get(netParams.at(0), 6)
                            val cellBF = AssertType(Float, "cellBF") $ Get(netParams.at(0), 7)
                            val cellBO = AssertType(Float, "cellBO") $ Get(netParams.at(0), 8)

                            def fusedDotProductWithNonLinear: Lambda =
                              fun(
                                /* w:   */ ArrayType(Float, d + h),
                                /* lut: */ ArrayType(Float, nLutValues),
                                /* b:   */ Float,
                                (w_, lut_, b_) => {
                                  val w = AssertType(ArrayType(Float, d + h), "cellW") $ w_
                                  val lut = AssertType(ArrayType(Float, nLutValues), "cellLUT") $ lut_
                                  val b = AssertType(Float, "cellB") $ b_

                                  // TODO: use LUT

                                  Zip(w, hPrevXCurSRAM) :>>
                                  SpPipeFold(chunkSize = rv, stride = rv, factor = ru,
                                    fMap = fun(
                                      ArrayType(TupleType(Float, Float), rv), wAndXhTile_ => {
                                        val wAndXhTile = AssertType(
                                          ArrayType(TupleType(Float, Float), rv), "wAndXhTile") $ wAndXhTile_

                                        wAndXhTile :>> SpPipeFold(chunkSize = 1, stride = 1, factor = rv,
                                          fMap = MapSeq(mult),
                                          fReduce = add,
                                          init = toReg(id) $ Value("0.0f", Float))
                                      }),
                                    fReduce = add,
                                    init = toReg(id) $ b) :>> toReg(fun(elem => lookup(elem, lut))) :>>
                                  AssertType(Float, "fusedDotProductWithNonLinear result")
                                })

                            val i = fusedDotProductWithNonLinear(cellWI, lutISRAM, cellBI)
                            val g = fusedDotProductWithNonLinear(cellWG, lutGSRAM, cellBG)
                            val f = fusedDotProductWithNonLinear(cellWF, lutFSRAM, cellBF)
                            val o = fusedDotProductWithNonLinear(cellWO, lutOSRAM, cellBO)

                            val newCellC_ = add(mult(i, g), mult(cellC, f))
                            // Compute newCellC once, and then pass the result to lambda output and
                            // to the expression computing new XH
                            newCellC_ :>> toReg(id) :>> Let(newCellC =>
                              Tuple(
                                /*c*/ toSRAM(id) $ newCellC,
                                /*h*/ toSRAM(mult)(toReg(lookup)(newCellC, lutTanhSRAM), o))) // TODO: Add the Tanh
                          })) :>> Unzip() :>>
                          fun(mapAccumBodyResult => {
                            val newC = Get(mapAccumBodyResult, 0)
                            val newH = SkipW(left=d, right=0) o toDRAM(id1D) $ Get(mapAccumBodyResult, 1)
                            Tuple(newC, newH)
                          }) :>>
                          AssertType(TupleType(ArrayType(Float, h), ArrayType(Float, d + h)),
                            "updated c and h (with an offset) of one time step")
                    })
                  })) :>>
//
                fun(mapAccumResult => {
                  val newCs = toDRAM(id1D) $ Get(mapAccumResult, 0)
                  val newHX = Get(mapAccumResult, 1) :>>
                    AssertType(ArrayType(ArrayType(Float, d + h), nSteps), "Updated h (with an offset)") :>>
                    PrintType() :>>
                    JoinW() :>> PrintType() :>>
                    SkipW(left=h, right=0) :>>
                    AssertType(ArrayType(Float, nSteps * (h + d) + h), "Updated hx")

                  Tuple(newCs, newHX)
                })
            })})})})})})})})})})})})})})})
        ))


    val runTimeLambda: Lambda = fun(
      /* xh:      */ ArrayType(Float, nSteps * (h + d) + h),
      /* c:       */ ArrayType(Float, h),
      /* wI:      */ ArrayType(ArrayType(Float, d + h), h),
      /* wC:      */ ArrayType(ArrayType(Float, d + h), h),
      /* wF:      */ ArrayType(ArrayType(Float, d + h), h),
      /* wO:      */ ArrayType(ArrayType(Float, d + h), h),
      /* bI:      */ ArrayType(Float, h),
      /* bC:      */ ArrayType(Float, h),
      /* bF:      */ ArrayType(Float, h),
      /* bO:      */ ArrayType(Float, h),
      /* lutI:    */ ArrayType(Float, nLutValues),
      /* lutC:    */ ArrayType(Float, nLutValues),
      /* lutF:    */ ArrayType(Float, nLutValues),
      /* lutO:    */ ArrayType(Float, nLutValues),
      /* lutTanh: */ ArrayType(Float, nLutValues),
      (xh, c,
       wI, wC, wF, wO,
       bI, bC, bF, bO,
       lutI, lutC, lutF, lutO, lutTanh) =>
        host.ir.ast.AccelFun(layerLambda)
        (xh, c,
          wI, wC, wF, wO,
          bI, bC, bF, bO,
          lutI, lutC, lutF, lutO, lutTanh)
    )

    val generatedSpatial = spatial.common.RuntimeCompiler(runTimeLambda)
    println(generatedSpatial)

    val expectedOutCode =
      """|{
         |    def id_3(x: SRAM1[Float]): SRAM1[Float] = {
         |        x
         |    }
         |    def id_2(x: Float): Float = {
         |        x
         |    }
         |    def id_1(x: DRAM2[Float]): DRAM2[Float] = {
         |        x
         |    }
         |    def id_0(x: DRAM1[Float]): DRAM1[Float] = {
         |        x
         |    }
         |    def mult(l: Float, r: Float): Float = {
         |        l * r
         |    }
         |    def add(x: Float, y: Float): Float = {
         |        x + y
         |    }
         |    def lookup(x: Float, lut: SRAM1[Float]): Float = {
         |        lut(((x + lutBound) / lutResolution).to[Int])
         |    }
         |    // reduce_seq
         |    // unroll
         |    val v__75 = SRAM[Float](v_h_1).buffer
         |    v__75 load id_0(v__57(0::v_h_1))
         |    val v__76 = SRAM[Float](v_h_1, (v_h_1 + v_d_2))
         |    v__76 load id_1(v__58(0::v_h_1, 0::(v_h_1 + v_d_2)))
         |    val v__77 = SRAM[Float](v_h_1, (v_h_1 + v_d_2))
         |    v__77 load id_1(v__59(0::v_h_1, 0::(v_h_1 + v_d_2)))
         |    val v__78 = SRAM[Float](v_h_1, (v_h_1 + v_d_2))
         |    v__78 load id_1(v__60(0::v_h_1, 0::(v_h_1 + v_d_2)))
         |    val v__79 = SRAM[Float](v_h_1, (v_h_1 + v_d_2))
         |    v__79 load id_1(v__61(0::v_h_1, 0::(v_h_1 + v_d_2)))
         |    val v__80 = SRAM[Float](v_h_1)
         |    v__80 load id_0(v__62(0::v_h_1))
         |    val v__81 = SRAM[Float](v_h_1)
         |    v__81 load id_0(v__63(0::v_h_1))
         |    val v__82 = SRAM[Float](v_h_1)
         |    v__82 load id_0(v__64(0::v_h_1))
         |    val v__83 = SRAM[Float](v_h_1)
         |    v__83 load id_0(v__65(0::v_h_1))
         |    val v__84 = SRAM[Float](v_nLutValues_6)
         |    v__84 load id_0(v__66(0::v_nLutValues_6))
         |    val v__85 = SRAM[Float](v_nLutValues_6)
         |    v__85 load id_0(v__67(0::v_nLutValues_6))
         |    val v__86 = SRAM[Float](v_nLutValues_6)
         |    v__86 load id_0(v__68(0::v_nLutValues_6))
         |    val v__87 = SRAM[Float](v_nLutValues_6)
         |    v__87 load id_0(v__69(0::v_nLutValues_6))
         |    val v__88 = SRAM[Float](v_nLutValues_6)
         |    v__88 load id_0(v__70(0::v_nLutValues_6))
         |    // mapAccum_seq
         |    Sequential.Foreach(0 until v_nSteps_0 by 1) { (v_i_34) =>
         |        val v__92 = SRAM[Float]((v_h_1 + v_d_2))
         |        v__92 load id_0(v__56((v_i_34 * (v_h_1 + v_d_2))::(v_h_1 + v_d_2 + (v_h_1 * v_i_34) + (v_d_2 * v_i_34))))
         |        val v__163 = SRAM[Float](v_h_1)
         |        // SpForeach
         |        Pipe.Foreach(0 until v_h_1 by 1 par v_hu_3) { (v_i_35) =>
         |            val v__94 = Reg[Float].buffer
         |            v__94 := id_2(v__80(v_i_35))
         |            Pipe.Fold(v__94)(0 until (v_h_1 + v_d_2) by v_rv_4 par v_ru_5) { (v_i_36) =>
         |                val v__98 = Reg[Float].buffer
         |                v__98 := id_2(0.0f)
         |                Pipe.Fold(v__98)(0 until v_rv_4 by 1 par v_rv_4) { (v_i_37) =>
         |                    val v__101_0 = Reg[Float]
         |                    // map_seq
         |                    // iteration count is exactly 1, no loop emitted
         |                    val v_i_38 = Reg[Int](0)
         |                    v__101_0 := mult(v__76(v_i_35, (v_i_36 + v_i_37)), v__92((v_i_36 + v_i_37)))
         |                    // end map_seq
         |                    v__101_0
         |                } {
         |                    add(_, _)
         |                }
         |                v__98
         |            } {
         |                add(_, _)
         |            }
         |            val v__107 = Reg[Float]
         |            v__107 := lookup(v__94, v__84(0::v_nLutValues_6))
         |            val v__108 = Reg[Float].buffer
         |            v__108 := id_2(v__81(v_i_35))
         |            Pipe.Fold(v__108)(0 until (v_h_1 + v_d_2) by v_rv_4 par v_ru_5) { (v_i_41) =>
         |                val v__112 = Reg[Float].buffer
         |                v__112 := id_2(0.0f)
         |                Pipe.Fold(v__112)(0 until v_rv_4 by 1 par v_rv_4) { (v_i_42) =>
         |                    val v__115_0 = Reg[Float]
         |                    // map_seq
         |                    // iteration count is exactly 1, no loop emitted
         |                    val v_i_43 = Reg[Int](0)
         |                    v__115_0 := mult(v__77(v_i_35, (v_i_41 + v_i_42)), v__92((v_i_41 + v_i_42)))
         |                    // end map_seq
         |                    v__115_0
         |                } {
         |                    add(_, _)
         |                }
         |                v__112
         |            } {
         |                add(_, _)
         |            }
         |            val v__121 = Reg[Float]
         |            v__121 := lookup(v__108, v__85(0::v_nLutValues_6))
         |            val v__123 = Reg[Float]
         |            v__123 := mult(v__107, v__121)
         |            val v__124 = Reg[Float].buffer
         |            v__124 := id_2(v__82(v_i_35))
         |            Pipe.Fold(v__124)(0 until (v_h_1 + v_d_2) by v_rv_4 par v_ru_5) { (v_i_46) =>
         |                val v__128 = Reg[Float].buffer
         |                v__128 := id_2(0.0f)
         |                Pipe.Fold(v__128)(0 until v_rv_4 by 1 par v_rv_4) { (v_i_47) =>
         |                    val v__131_0 = Reg[Float]
         |                    // map_seq
         |                    // iteration count is exactly 1, no loop emitted
         |                    val v_i_48 = Reg[Int](0)
         |                    v__131_0 := mult(v__78(v_i_35, (v_i_46 + v_i_47)), v__92((v_i_46 + v_i_47)))
         |                    // end map_seq
         |                    v__131_0
         |                } {
         |                    add(_, _)
         |                }
         |                v__128
         |            } {
         |                add(_, _)
         |            }
         |            val v__137 = Reg[Float]
         |            v__137 := lookup(v__124, v__86(0::v_nLutValues_6))
         |            val v__139 = Reg[Float]
         |            v__139 := mult(v__75(v_i_35), v__137)
         |            val v__141 = Reg[Float]
         |            v__141 := add(v__123, v__139)
         |            val v__142 = Reg[Float]
         |            v__142 := id_2(v__141)
         |            v__75(v_i_35) = id_2(v__142)
         |            val v__146 = Reg[Float]
         |            v__146 := lookup(v__142, v__88(0::v_nLutValues_6))
         |            val v__147 = Reg[Float].buffer
         |            v__147 := id_2(v__83(v_i_35))
         |            Pipe.Fold(v__147)(0 until (v_h_1 + v_d_2) by v_rv_4 par v_ru_5) { (v_i_51) =>
         |                val v__151 = Reg[Float].buffer
         |                v__151 := id_2(0.0f)
         |                Pipe.Fold(v__151)(0 until v_rv_4 by 1 par v_rv_4) { (v_i_52) =>
         |                    val v__154_0 = Reg[Float]
         |                    // map_seq
         |                    // iteration count is exactly 1, no loop emitted
         |                    val v_i_53 = Reg[Int](0)
         |                    v__154_0 := mult(v__79(v_i_35, (v_i_51 + v_i_52)), v__92((v_i_51 + v_i_52)))
         |                    // end map_seq
         |                    v__154_0
         |                } {
         |                    add(_, _)
         |                }
         |                v__151
         |            } {
         |                add(_, _)
         |            }
         |            val v__160 = Reg[Float]
         |            v__160 := lookup(v__147, v__87(0::v_nLutValues_6))
         |            v__163(v_i_35) = mult(v__146, v__160)
         |        }
         |        // end SpForeach
         |        v__56((v_h_1 + v_d_2 + (v_h_1 * v_i_34) + (v_d_2 * v_i_34))::(v_d_2 + (2 * v_h_1) + (v_h_1 * v_i_34) + (v_d_2 * v_i_34))) store id_3(v__163(0::v_h_1))
         |    }
         |    // end mapAccum_seq
         |    v__57(0::v_h_1) store id_3(v__75(0::v_h_1))
         |    // end unroll
         |    // end reduce_seq
         |}""".stripMargin


    val cleanedGeneratedSpatial = cleanSpatialProgram(generatedSpatial)
    val cleanedExpectedOutCode = cleanSpatialProgram(expectedOutCode)

    assertEquals(cleanedExpectedOutCode, cleanedGeneratedSpatial)

    NewFactorizationOfSum.enabled = originalNewFactorizationEnabledStatus
  }
}
