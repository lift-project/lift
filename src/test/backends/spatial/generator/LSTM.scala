package backends.spatial.generator

import arithmetic.TypeVar
import backends.spatial.accel.ir.pattern.{ReduceSeq, SpPipeFold, SpPipeForeach, toSRAM}
import backends.spatial.common.ir._
import backends.spatial.host
import backends.spatial.accel.ir._
import backends.{Backend, spatial}
import ir.ast.debug.AssertType
import ir.ast.{Expr, Gather, Get, Head, Join, Lambda, Let, Param, Slide, Split, Tail, Tuple, UserFun, Value, Zip, fun, reverse}
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
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

    Backend.setSpatial()

    val nSteps = SizeVar("nSteps")
    val nCells = SizeVar("nCells")
    val nFeatures = SizeVar("nFeatures")
    val hu = SizeVar("hu")
    val rv = SizeVar("rv")
    val ru = SizeVar("ru")
    val nLutValues = SizeVar("nLutValues")

    val xhSize = (nFeatures + nCells) * (nSteps + 1)

    def layerLambda: Lambda = fun(
      /* xh:      */ ArrayType(Float, xhSize),
      /* c:       */ ArrayType(Float, nCells),
      /* wI:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* wG:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* wF:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* wO:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* bI:      */ ArrayType(Float, nCells),
      /* bG:      */ ArrayType(Float, nCells),
      /* bF:      */ ArrayType(Float, nCells),
      /* bO:      */ ArrayType(Float, nCells),
      /* lutI:    */ ArrayType(Float, nLutValues),
      /* lutG:    */ ArrayType(Float, nLutValues),
      /* lutF:    */ ArrayType(Float, nLutValues),
      /* lutO:    */ ArrayType(Float, nLutValues),
      /* lutTanh: */ ArrayType(Float, nLutValues),
      (xh, c,
       wI, wG, wF, wO,
       bI, bG, bF, bO,
       lutI, lutG, lutF, lutO, lutTanh) =>

        xh :>> toSRAM(id1D) :>> Let(xhSRAM => {
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

          val xhInitialStep =
            AssertType(ArrayType(Float, nFeatures + nCells), "xhInitialStep") o
              Join() o Head() o Split(nFeatures + nCells) $ xhSRAM

          val xhNextSteps =
            AssertType(ArrayType(ArrayType(Float, nFeatures + nCells), nSteps), "xhNextSteps") o
              Tail() o Split(nFeatures + nCells) $ xhSRAM

          xhNextSteps :>>
          ScanSeq(fun(
            /* xh from previous step: */ArrayType(Float, nFeatures + nCells),
            /* xh from the next step: */ArrayType(Float, nFeatures + nCells),
            (xhPrevStep_, xhNextStep) => {
              // Get the h from xh (this is a convoluted way of performing a slice):
              val hNextStep =
                // 1. Reverse the order of xhNextStep
                xhNextStep :>> Gather(reverse) :>>
                // 2. Slide a window of size nCells across xhNextStep.
                Slide(nCells, nCells) :>>
                // 3. Get only the first window, which is reversed h
                Head() :>> Join() :>>
                // 4. Restore the order of elements within h
                Gather(reverse) :>> AssertType(ArrayType(Float, nCells), "hNextStep")

              // Write into hNextStep by reducing the computation "into" it.
              Value("0.0f", ArrayType(Float, 1)) :>> ReduceSeq(
                init = hNextStep,
                f = fun((_, _) => {

                  val xhPrevStep = AssertType(ArrayType(Float, nFeatures + nCells), "xhPrevStep") $ xhPrevStep_

                  Zip(cSRAM, wISRAM, wGSRAM, wFSRAM, wOSRAM, bISRAM, bGSRAM, bFSRAM, bOSRAM) :>>
                    //
                    SpPipeForeach(chunkSize = 1, stride = 1, factor = hu,
                      f = fun(netParams => {
                        val cellC = AssertType(Float, "cellC") $ Get(Join() $ netParams, 0)
                        val cellWI = AssertType(ArrayType(Float, nFeatures + nCells), "cellWI") $ Get(Join() $ netParams, 1)
                        val cellWG = AssertType(ArrayType(Float, nFeatures + nCells), "cellWG") $ Get(Join() $ netParams, 2)
                        val cellWF = AssertType(ArrayType(Float, nFeatures + nCells), "cellWF") $ Get(Join() $ netParams, 3)
                        val cellWO = AssertType(ArrayType(Float, nFeatures + nCells), "cellWO") $ Get(Join() $ netParams, 4)
                        val cellBI = AssertType(Float, "cellBI") $ Get(Join() $ netParams, 5)
                        val cellBG = AssertType(Float, "cellBG") $ Get(Join() $ netParams, 6)
                        val cellBF = AssertType(Float, "cellBF") $ Get(Join() $ netParams, 7)
                        val cellBO = AssertType(Float, "cellBO") $ Get(Join() $ netParams, 8)

                        def fusedDotProductWithNonLinear: Lambda =
                          fun(
                            /* w:   */ ArrayType(Float, nFeatures + nCells),
                            /* lut: */ ArrayType(Float, nLutValues),
                            /* b:   */ Float,
                            (w_, lut_, b_) => {
                              val w = AssertType(ArrayType(Float, nFeatures + nCells), "cellW") $ w_
                              val lut = AssertType(ArrayType(Float, nLutValues), "cellLUT") $ lut_
                              val b = AssertType(Float, "cellB") $ b_

                              // TODO: use LUT

                              Zip(w, xhPrevStep) :>>
                                SpPipeFold(chunkSize = rv, stride = rv, factor = ru,
                                  fMap = fun(
                                    ArrayType(TupleType(Float, Float), rv), wAndXhTile_ => {
                                      val wAndXhTile = AssertType(ArrayType(TupleType(Float, Float), rv), "wAndXhTile") $ wAndXhTile_

                                      wAndXhTile :>> SpPipeFold(chunkSize = 1, stride = 1, factor = rv,
                                        fMap = mult,
                                        fReduce = add,
                                        init = Value("0.0f", Float))
                                    }),
                                  fReduce = add,
                                  init = b) // TODO: make sure we are not writing into bias here
                            })

                        val i = fusedDotProductWithNonLinear(cellWI, lutISRAM, cellBI)
                        val g = fusedDotProductWithNonLinear(cellWG, lutGSRAM, cellBG)
                        val f = fusedDotProductWithNonLinear(cellWF, lutFSRAM, cellBF)
                        val o = fusedDotProductWithNonLinear(cellWO, lutOSRAM, cellBO)

                        val newCellC_ = add(mult(i, g), mult(cellC, f))
                        // Compute newCellC once, and then pass result to lambda output and
                        // to the expression computing new XH
                        newCellC_ :>> Let(newCellC =>
                          Tuple(newCellC, mult(newCellC, o)) // TODO: Add the Tanh
                        )
                      }))
                }))

            }),

            init = xhInitialStep
          ) :>> Join()

        })})})})})})})})})})})})})})})

        )


    val runTimeLambda: Lambda = fun(
      /* xh:      */ ArrayType(Float, xhSize),
      /* c:       */ ArrayType(Float, nCells),
      /* wI:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* wC:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* wF:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* wO:      */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
      /* bI:      */ ArrayType(Float, nCells),
      /* bC:      */ ArrayType(Float, nCells),
      /* bF:      */ ArrayType(Float, nCells),
      /* bO:      */ ArrayType(Float, nCells),
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

  }
}
