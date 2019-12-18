package exploration.spatial

import arithmetic.TypeVar
import backends.Backend
import backends.spatial.accel.ir.pattern.MapAccumSeq
import backends.spatial.accel.ir._
import backends.spatial.common.ir.Float
import _root_.ir.ast.debug.AssertType
import _root_.ir.{ArrayType, TupleType, TypeChecker}
import _root_.ir.ast.{ArrayAccess, Concat, Drop, Get, Lambda, Map, Reduce, Split, Tuple, Unzip, UserFun, Zip, fun}
import lift.arithmetic.{NewFactorizationOfSum, SizeVar}
import org.junit.{AfterClass, BeforeClass, Test}

object LSTM {
  var originalNewFactorizationEnabledStatus: Boolean = _

  @BeforeClass def before(): Unit = {
    originalNewFactorizationEnabledStatus = NewFactorizationOfSum()
    NewFactorizationOfSum.enabled = true
    Backend.setSpatial()
  }

  @AfterClass def after(): Unit = {
    NewFactorizationOfSum.enabled = originalNewFactorizationEnabledStatus
    Backend.setOpenCL()
  }
}


class TestRewriteLSTM {

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

  def fusedDotProductWithNonLinear: Lambda = fun(
    /* xCur:  */ ArrayType(Float, d),
    /* hPrev: */ ArrayType(Float, h),
    /* wi:    */ ArrayType(Float, d),
    /* wh:    */ ArrayType(Float, h),
    /* lut:   */ ArrayType(Float, nLutValues),
    /* b:     */ Float,
    (xCur_, hPrev_, wi_, wh_, lut_, b_) => {
      val xCur = AssertType(ArrayType(Float, d), "xCur") $ xCur_
      val hPrev = AssertType(ArrayType(Float, h), "hPrev") $ hPrev_
      val wi = AssertType(ArrayType(Float, d), "cellWi") $ wi_
      val wh = AssertType(ArrayType(Float, h), "cellWh") $ wh_
      val lut = AssertType(ArrayType(Float, nLutValues), "cellLUT") $ lut_
      val b = AssertType(Float, "cellB") $ b_

      Zip(Concat(wi, wh), Concat(xCur, hPrev)) :>>
      Map(fun(pair => mult(pair._0, pair._1))) :>> Reduce(add, init = b) :>>
      ArrayAccess(0) :>>
      fun(elem => lookup(elem, lut)) :>>
      AssertType(Float, "fusedDotProductWithNonLinear result")
    })

  def highLevelLSTMLambda: Lambda = fun(
    /* x:       */ ArrayType(ArrayType(Float, d), nSteps),
    /* hInit:   */ ArrayType(Float, h),
    /* c:       */ ArrayType(Float, h),
    /* wIi:     */ ArrayType(ArrayType(Float, d), h),
    /* wGi:     */ ArrayType(ArrayType(Float, d), h),
    /* wFi:     */ ArrayType(ArrayType(Float, d), h),
    /* wOi:     */ ArrayType(ArrayType(Float, d), h),
    /* wIh:     */ ArrayType(ArrayType(Float, h), h),
    /* wGh:     */ ArrayType(ArrayType(Float, h), h),
    /* wFh:     */ ArrayType(ArrayType(Float, h), h),
    /* wOh:     */ ArrayType(ArrayType(Float, h), h),
    /* bI:      */ ArrayType(Float, h),
    /* bG:      */ ArrayType(Float, h),
    /* bF:      */ ArrayType(Float, h),
    /* bO:      */ ArrayType(Float, h),
    /* lutI:    */ ArrayType(Float, nLutValues),
    /* lutG:    */ ArrayType(Float, nLutValues),
    /* lutF:    */ ArrayType(Float, nLutValues),
    /* lutO:    */ ArrayType(Float, nLutValues),
    /* lutTanh: */ ArrayType(Float, nLutValues),
    (x, hInit, c,
     wIi, wGi, wFi, wOi,
     wIh, wGh, wFh, wOh,
     bI, bG, bF, bO,
     lutI, lutG, lutF, lutO, lutTanh) =>
  //
      x :>>
      MapAccumSeq(
        init = Tuple(c, hInit),

        f = fun(
          /* c and h of the previous step */
          TupleType(ArrayType(Float, h), ArrayType(Float, h)),
          /* x of the current step: */
          ArrayType(Float, d),
          (chPrev, xCur_) => {
            val cPrev = AssertType(ArrayType(Float, h), "cPrev") $ Get(chPrev, 0)
            val hPrev = AssertType(ArrayType(Float, h), "hPrev") $ Get(chPrev, 1)
            val xCur = AssertType(ArrayType(Float, d), "xCur") $ xCur_

            Zip(cPrev, wIi, wGi, wFi, wOi, wIh, wGh, wFh, wOh, bI, bG, bF, bO) :>>
            Map(fun(netParams => {
              val cellC = AssertType(Float, "cellC") $ Get(netParams, 0)
              val cellWIi = AssertType(ArrayType(Float, d), "cellWIi") $ Get(netParams, 1)
              val cellWGi = AssertType(ArrayType(Float, d), "cellWGi") $ Get(netParams, 2)
              val cellWFi = AssertType(ArrayType(Float, d), "cellWFi") $ Get(netParams, 3)
              val cellWOi = AssertType(ArrayType(Float, d), "cellWOi") $ Get(netParams, 4)
              val cellWIh = AssertType(ArrayType(Float, h), "cellWIh") $ Get(netParams, 5)
              val cellWGh = AssertType(ArrayType(Float, h), "cellWGh") $ Get(netParams, 6)
              val cellWFh = AssertType(ArrayType(Float, h), "cellWFh") $ Get(netParams, 7)
              val cellWOh = AssertType(ArrayType(Float, h), "cellWOh") $ Get(netParams, 8)
              val cellBI = AssertType(Float, "cellBI") $ Get(netParams, 9)
              val cellBG = AssertType(Float, "cellBG") $ Get(netParams, 10)
              val cellBF = AssertType(Float, "cellBF") $ Get(netParams, 11)
              val cellBO = AssertType(Float, "cellBO") $ Get(netParams, 12)

              val i = fusedDotProductWithNonLinear(xCur, hPrev, cellWIi, cellWIh, lutI, cellBI)
              val g = fusedDotProductWithNonLinear(xCur, hPrev, cellWGi, cellWGh, lutG, cellBG)
              val f = fusedDotProductWithNonLinear(xCur, hPrev, cellWFi, cellWFh, lutF, cellBF)
              val o = fusedDotProductWithNonLinear(xCur, hPrev, cellWOi, cellWOh, lutO, cellBO)

              val newCellC = add(mult(i, g), mult(cellC, f))

              Tuple(/* c */ newCellC, /* h */ mult(lookup(newCellC, lutTanh), o)) :>>
              AssertType(TupleType(Float, Float), "Updated c and h of a cell")
            })) :>> Unzip() :>>
              fun(mapAccumBodyResult => {
                val newC = Get(mapAccumBodyResult, 0)
                val newH = Get(mapAccumBodyResult, 1)
                Tuple(Tuple(newC, newH), newH)
              }) :>>
              AssertType(
                TupleType(TupleType(ArrayType(Float, h), ArrayType(Float, h)),
                ArrayType(Float, h)), "Updated c and h of all steps of one time step")
          })) :>>
      fun(mapAccumResult => {
        val newC = Get(Get(mapAccumResult, 0), 0)
        val newHs = Get(mapAccumResult, 1)
        Tuple(newC, newHs)
      })
  )

  @Test
  def rewriteLSTM(): Unit = {
    TypeChecker(highLevelLSTMLambda.body)
  }
}
