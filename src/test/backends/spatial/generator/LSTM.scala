package backends.spatial.generator

import arithmetic.TypeVar
import backends.spatial.accel.ir.pattern.{ReduceSeq, SpPipeFold, SpPipeForeach, toSRAM}
import backends.spatial.common.ir._
import backends.spatial.host
import backends.{Backend, spatial}
import ir.ast.debug.AssertType
import ir.ast.{Expr, Head, Join, Lambda, Let, Param, Split, Tail, UserFun, Value, Zip, fun}
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
          ReduceSeq(fun((acc, xh_) => {

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

              Join() o
                ScanSeq(
                  fun((acc, xhStep) => AssertType(ArrayType(Float, nFeatures + nCells)) $ xhStep),
                  init = xhPrevStep
                ) $ xhNextSteps
            })})})})})})})})})})})})})})})

          }),
            // Write back to xh, starting from index nFeatures + nCells (to skip the previous step):
            init = Join() o Tail() o Split(nFeatures + nCells) $ xh
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
