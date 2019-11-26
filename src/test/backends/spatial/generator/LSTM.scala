package backends.spatial.generator

import arithmetic.TypeVar
import backends.spatial.accel.ir.pattern.{MapAccumSeq, MapSeq, ReduceSeq, SpPipeFold, SpPipeForeach, toReg, toSRAM}
import backends.spatial.common.ir._
import backends.spatial.host
import backends.spatial.accel.ir._
import backends.{Backend, spatial}
import ir.ast.debug.{AssertType, PrintType}
import ir.ast.{ArrayAccess, Expr, Gather, Get, Head, Join, Lambda, Let, Map, Param, Slice, Slide, Split, Tail, Tuple, Unzip, UserFun, Value, Zip, fun, reverse}
import ir.{ArrayType, TupleType}
import lift.arithmetic.{NewFactorizationOfSum, SizeVar}
import opencl.ir.pattern.ScanSeq
import org.junit.{AfterClass, Test}

object LSTM {
  @AfterClass def after(): Unit = {
    Backend.setOpenCL()
  }
}

class LSTM {
  val x = TypeVar()
  val y = TypeVar()

  val id = UserFun("id", Array("x"), "x", Seq(Float), Float)
  val id1D = UserFun("id", Array("x"), "x", Seq(ArrayType(Float, x)), ArrayType(Float, x))
  val id2D = UserFun("id", Array("x"), "x", Seq(ArrayType(ArrayType(Float, x), y)), ArrayType(ArrayType(Float, x), y))

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

    val hxSize = nSteps * (h + d) + h

    def layerLambda: Lambda = fun(
      /* xh:      */ ArrayType(Float, hxSize),
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
          init = Tuple(c,
            hx :>> Slice(h+d, hxSize) :>> Slide(h, h + d) :>> Join() :>>
            AssertType(ArrayType(Float, h * nSteps), "h sectors of hx in DRAM")),
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
//            lutI :>> toSRAM(id1D) :>> Let(lutISRAM => {
//            lutG :>> toSRAM(id1D) :>> Let(lutGSRAM => {
//            lutF :>> toSRAM(id1D) :>> Let(lutFSRAM => {
//            lutO :>> toSRAM(id1D) :>> Let(lutOSRAM => {
//            lutTanh :>> toSRAM(id1D) :>> Let(lutTanhSRAM => {

              hx :>> Slice(0, hxSize - h) :>> Split(h+d) :>>
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
//                                /* lut: */ ArrayType(Float, nLutValues),
                                /* b:   */ Float,
                                (w_, /*lut_,*/ b_) => {
                                  val w = AssertType(ArrayType(Float, d + h), "cellW") $ w_
//                                  val lut = AssertType(ArrayType(Float, nLutValues), "cellLUT") $ lut_
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
                                    init = toReg(id) $ b) :>>
                                  AssertType(Float, "fusedDotProductWithNonLinear result")
                                })

//                            val i = fusedDotProductWithNonLinear(cellWI, lutISRAM, cellBI)
//                            val g = fusedDotProductWithNonLinear(cellWG, lutGSRAM, cellBG)
//                            val f = fusedDotProductWithNonLinear(cellWF, lutFSRAM, cellBF)
//                            val o = fusedDotProductWithNonLinear(cellWO, lutOSRAM, cellBO)
                            val i = fusedDotProductWithNonLinear(cellWI, cellBI)
                            val g = fusedDotProductWithNonLinear(cellWG, cellBG)
                            val f = fusedDotProductWithNonLinear(cellWF, cellBF)
                            val o = fusedDotProductWithNonLinear(cellWO, cellBO)

                            val newCellC_ = toReg(add)(mult(i, g), mult(cellC, f))
                            // Compute newCellC once, and then pass result to lambda output and
                            // to the expression computing new XH
                            newCellC_ :>> Let(newCellC =>
                              Tuple(/*c*/ newCellC, /*h*/ mult(newCellC, o))) // TODO: Add the Tanh
                          })) :>> Unzip() :>>
                        AssertType(TupleType(ArrayType(Float, h), ArrayType(Float, h)), "c and h of one time step")
                    })
                  })) :>>
                // Just changing the view
                fun(mapAccumResult => {
                  val newCs = Get(mapAccumResult, 0)
                  val newHs = Get(mapAccumResult, 1)
                  Tuple(newCs, Join() $ newHs)
                })
            })})})})})})})})})})//})})})})//})
        ))


    val runTimeLambda: Lambda = fun(
      /* xh:      */ ArrayType(Float, hxSize),
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

    NewFactorizationOfSum.enabled = originalNewFactorizationEnabledStatus
  }
}
