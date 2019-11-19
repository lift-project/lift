package backends.spatial.generator

import arithmetic.TypeVar
import backends.spatial.accel.ir.pattern.{ReduceSeq, SpPipeFold, SpPipeForeach, toSRAM}
import backends.spatial.common.ir._
import backends.spatial.host
import backends.{Backend, spatial}
import ir.ast.debug.AssertType
import ir.ast.{Expr, Get, Head, Join, Lambda, Let, Param, Split, Tail, UserFun, Value, Zip, fun}
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
        // Wrap xh into array of 1
        xh :>> Split(xhSize) :>>
          ReduceSeq(
            // Write back to xh, starting from index nFeatures + nCells (to skip the previous step):
            init = Join() o Tail() o Split(nFeatures + nCells) $ xh,

            fun((acc, xh_) => {

            xh_ :>> toSRAM(id1D) :>> Let(xhSRAM => {
            c :>> toSRAM(id1D) :>> Let(cSRAM => {
            wI :>> toSRAM(id2D) :>> Let(wISRAM => {
            wC :>> toSRAM(id2D) :>> Let(wCSRAM => {
            wF :>> toSRAM(id2D) :>> Let(wFSRAM => {
            wO :>> toSRAM(id2D) :>> Let(wOSRAM => {
            bI :>> toSRAM(id1D) :>> Let(bISRAM => {
            bC :>> toSRAM(id1D) :>> Let(bCSRAM => {
            bF :>> toSRAM(id1D) :>> Let(bFSRAM => {
            bO :>> toSRAM(id1D) :>> Let(bOSRAM => {
            lutI :>> toSRAM(id1D) :>> Let(lutISRAM => {
            lutC :>> toSRAM(id1D) :>> Let(lutCSRAM => {
            lutF :>> toSRAM(id1D) :>> Let(lutFSRAM => {
            lutO :>> toSRAM(id1D) :>> Let(lutOSRAM => {
            lutTanh :>> toSRAM(id1D) :>> Let(lutTanhSRAM => {

              val xhPrevStep =
                AssertType(ArrayType(Float, nFeatures + nCells), "xhPrevStep") o
                  Join() o Head() o Split(nFeatures + nCells) $ xhSRAM

              val xhNextSteps =
                AssertType(ArrayType(ArrayType(Float, nFeatures + nCells), nSteps), "xhNextSteps") o
                  Tail() o Split(nFeatures + nCells) $ xhSRAM

              Join() o ScanSeq(fun(
                /* xh from previous step: */ArrayType(Float, nFeatures + nCells),
                /* xh from the next step: */ArrayType(Float, nFeatures + nCells),
                (acc, xhNextStep) =>
                    Zip(wISRAM, wCSRAM, wFSRAM, wOSRAM, bISRAM, bCSRAM, bFSRAM, bOSRAM) :>>

                    SpPipeForeach(chunkSize = 1, iterSize = 1, factor = hu,
                      f = fun(netParams => {
                        val cellWI = AssertType(ArrayType(Float, nFeatures + nCells), "cellWI") $ Get(Join() $ netParams, 0)
                        val cellWC = AssertType(ArrayType(Float, nFeatures + nCells), "cellWC") $ Get(Join() $ netParams, 1)
                        val cellWF = AssertType(ArrayType(Float, nFeatures + nCells), "cellWF") $ Get(Join() $ netParams, 2)
                        val cellWO = AssertType(ArrayType(Float, nFeatures + nCells), "cellWO") $ Get(Join() $ netParams, 3)
                        val cellBI = AssertType(Float, "cellBI") $ Get(Join() $ netParams, 4)
                        val cellBC = AssertType(Float, "cellBC") $ Get(Join() $ netParams, 5)
                        val cellBF = AssertType(Float, "cellBF") $ Get(Join() $ netParams, 6)
                        val cellBO = AssertType(Float, "cellBO") $ Get(Join() $ netParams, 7)
//                  xhNextStep :>> AssertType(ArrayType(Float, nFeatures + nCells)) :>>
                        def fusedDotProductWithNonLinear: Expr =
                          fun(
                            /* w:   */ ArrayType(ArrayType(Float, nFeatures + nCells), nCells),
                            /* lut: */ ArrayType(Float, nLutValues),
                            /* b:   */ ArrayType(Float, nCells),
                            (w, lut, b) =>
                              SpPipeFold(chunkSize = rv, stride = rv, factor = ru,
                                fMap = fun(

                                ),
                                fReduce = {},
                                init = Value("0.0f", Float))
                          )

                      })
                    )
                  ),

                init = xhPrevStep
              ) $ xhNextSteps

            })})})})})})})})})})})})})})})

          })
          ))


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
