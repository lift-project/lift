package exploration.spatial

import arithmetic.TypeVar
import backends.Backend
import backends.spatial.accel.ir.pattern.{MapAccumSeq, ReduceSeq}
import backends.spatial.accel.ir._
import backends.spatial.common.ir.Float
import _root_.ir.ast.debug.{AssertType}
import _root_.ir.{ArrayType, TupleType, TypeChecker}
import _root_.ir.ast.{ArrayAccess, Concat, Get, Lambda, Let, Map, Reduce, Tuple, Unzip, UserFun, Value, Zip, fun}
import lift.arithmetic.{NewFactorizationOfSum, SizeVar}
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.Rewrite
import rewriting.rules.{FactorizationRules, FissionRules, SimplificationRules}
import rewriting.utils.{NumberExpression, NumberPrinter}

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
      def xCur = AssertType(ArrayType(Float, d), "xCur") $ xCur_
      def hPrev = AssertType(ArrayType(Float, h), "hPrev") $ hPrev_
      def wi = AssertType(ArrayType(Float, d), "cellWi") $ wi_
      def wh = AssertType(ArrayType(Float, h), "cellWh") $ wh_
      def lut = AssertType(ArrayType(Float, nLutValues), "cellLUT") $ lut_
      def b = AssertType(Float, "cellB") $ b_

      Zip(Concat(wi, wh), Concat(xCur, hPrev)) :>>
      Map(fun(pair => mult(pair._0, pair._1))) :>> Reduce(add, init = b) :>>
      ArrayAccess(0) :>>
      fun(elem => lookup(elem, lut)) :>>
      AssertType(Float, "fusedDotProductWithNonLinear result")
    })

  val highLevelLSTMLambda: Lambda = fun(
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
            def cPrev = AssertType(ArrayType(Float, h), "cPrev") $ Get(chPrev, 0)
            def hPrev = AssertType(ArrayType(Float, h), "hPrev") $ Get(chPrev, 1)
            def xCur = AssertType(ArrayType(Float, d), "xCur") $ xCur_

            Zip(cPrev, wIi, wGi, wFi, wOi, wIh, wGh, wFh, wOh, bI, bG, bF, bO) :>>
            Map(fun(netParams => {
              def cellC = AssertType(Float, "cellC") $ Get(netParams, 0)
              def cellWIi = AssertType(ArrayType(Float, d), "cellWIi") $ Get(netParams, 1)
              def cellWGi = AssertType(ArrayType(Float, d), "cellWGi") $ Get(netParams, 2)
              def cellWFi = AssertType(ArrayType(Float, d), "cellWFi") $ Get(netParams, 3)
              def cellWOi = AssertType(ArrayType(Float, d), "cellWOi") $ Get(netParams, 4)
              def cellWIh = AssertType(ArrayType(Float, h), "cellWIh") $ Get(netParams, 5)
              def cellWGh = AssertType(ArrayType(Float, h), "cellWGh") $ Get(netParams, 6)
              def cellWFh = AssertType(ArrayType(Float, h), "cellWFh") $ Get(netParams, 7)
              def cellWOh = AssertType(ArrayType(Float, h), "cellWOh") $ Get(netParams, 8)
              def cellBI = AssertType(Float, "cellBI") $ Get(netParams, 9)
              def cellBG = AssertType(Float, "cellBG") $ Get(netParams, 10)
              def cellBF = AssertType(Float, "cellBF") $ Get(netParams, 11)
              def cellBO = AssertType(Float, "cellBO") $ Get(netParams, 12)

              def i = fusedDotProductWithNonLinear(xCur, hPrev, cellWIi, cellWIh, lutI, cellBI)
              def g = fusedDotProductWithNonLinear(xCur, hPrev, cellWGi, cellWGh, lutG, cellBG)
              def f = fusedDotProductWithNonLinear(xCur, hPrev, cellWFi, cellWFh, lutF, cellBF)
              def o = fusedDotProductWithNonLinear(xCur, hPrev, cellWOi, cellWOh, lutO, cellBO)

              def newCellC = add(mult(i, g), mult(cellC, f))

              Tuple(/* c */ newCellC, /* h */ mult(lookup(newCellC, lutTanh), o)) :>>
              AssertType(TupleType(Float, Float), "Updated c and h of a cell")
            })) :>> Unzip() :>>
              fun(mapAccumBodyResult => {
                def newC = Get(mapAccumBodyResult, 0)
                def newH = Get(mapAccumBodyResult, 1)
                Tuple(Tuple(newC, newH), newH)
              }) :>>
              AssertType(
                TupleType(TupleType(ArrayType(Float, h), ArrayType(Float, h)),
                ArrayType(Float, h)), "Updated c and h of all steps of one time step")
          })) :>>
      fun(mapAccumResult => {
        def newC = Get(Get(mapAccumResult, 0), 0)
        def newHs = Get(mapAccumResult, 1)
        Tuple(newC, newHs)
      })
  )

  def printExprIDs(lambda: Lambda): Unit = {
    val printAllIDs = true
    println("-------------------------------")

    val numbering = NumberExpression.breadthFirst(lambda)

//    println("Concat of cellWIi and cellWIh:")
//    println(Rewrite.getExprForId(highLevelLSTMLambda.body, idConcat_cellWIi_cellWIh, numbering).toString)

    if (printAllIDs) {
      println("Subexpression IDs:")
      println(NumberPrinter(lambda))
    }
    println("-------------------------------")
  }

  @Test
  def rewriteLSTM(): Unit = {
    TypeChecker(highLevelLSTMLambda.body)

//    printExprIDs(highLevelLSTMLambda)

    // Sequence
    val f1_idConcat_cellWIi_cellWIh = 281
    val f2_concatWIicurried = Rewrite.applyRuleAtId(highLevelLSTMLambda, f1_idConcat_cellWIi_cellWIh, SimplificationRules.curryFun)
//    println(f2_concatWIcurried)

//    printExprIDs(f2_concatWIicurried)

    val f2_idConcat_cellWIiParam_cellWIh = 286
    val f3_concatWIhcurried = Rewrite.applyRuleAtId(f2_concatWIicurried, f2_idConcat_cellWIiParam_cellWIh, SimplificationRules.curryFun)

    println(f3_concatWIhcurried)

    val potentialRewrites0 = Rewrite.listAllPossibleRewritesForRules(
      lambda = f3_concatWIhcurried,
      rules = Seq(FactorizationRules.commonSubExprInTuples))
    println(f"Potential rewrites: ${potentialRewrites0.length}")

    val rewritten0 = potentialRewrites0.map(potentialRewrite =>
      Rewrite.applyRuleAt(
        lambda = f3_concatWIhcurried,
        expr = potentialRewrite.expr,
        rule = potentialRewrite.rule))
    println(rewritten0)
    return

    val potentialRewrites = Rewrite.listAllPossibleRewritesForRules(
      lambda = f3_concatWIhcurried,
      rules = Seq(FissionRules.mapFissionWithZipOutside_v2))
    println(f"Potential rewrites: ${potentialRewrites.length}")

    val rewritten = potentialRewrites.map(potentialRewrite =>
      Rewrite.applyRuleAt(
        lambda = f3_concatWIhcurried,
        expr = potentialRewrite.expr,
        rule = potentialRewrite.rule))

    println(rewritten)
  }

  @Test
  def temp(): Unit = {
    val toyLambda: Lambda = fun(
      /* x:       */ ArrayType(Float, d),
      /* hInit:   */ ArrayType(Float, h),
      /* wIi:     */ ArrayType(ArrayType(Float, d), h),
      /* wIh:     */ ArrayType(ArrayType(Float, h), h),
      /* bI:      */ ArrayType(Float, h),
      /* lutI:    */ ArrayType(Float, nLutValues),
      (xCur, hInit,
       wIi,
       wIh,
       bI,
       lutI) =>

        Zip(wIi, wIh, bI) :>>
          Map(fun(netParams => {
            def cellWIi = AssertType(ArrayType(Float, d), "cellWIi") $ Get(netParams, 0)
            def cellWIh = AssertType(ArrayType(Float, h), "cellWIh") $ Get(netParams, 1)
            def cellBI = AssertType(Float, "cellBI") $ Get(netParams, 2)

            def newCellC = cellWIh :>> Let(p2 => cellWIi :>> Let(p => Concat(p, p2))) :>>
              ReduceSeq(add, Value("0", Float)) :>> ArrayAccess(0) :>> id

//            newCellC :>> Let(p => Tuple(/* c */ p, /* h */ p))
            Tuple(/* c */ newCellC, /* h */ newCellC)
          })))

    val potentialRewrites0 = Rewrite.listAllPossibleRewritesForRules(
      lambda = toyLambda,
      rules = Seq(FactorizationRules.commonSubExprInTuples))
    println(f"Potential rewrites: ${potentialRewrites0.length}")

    val rewritten0 = potentialRewrites0.map(potentialRewrite =>
      Rewrite.applyRuleAt(
        lambda = toyLambda,
        expr = potentialRewrite.expr,
        rule = potentialRewrite.rule))
    println(rewritten0)
    return
//    printExprIDs(toyLambda)

    val potentialRewrites = Rewrite.listAllPossibleRewritesForRules(
      lambda = toyLambda,
      rules = Seq(FissionRules.mapFissionWithZipOutside))
    println(f"Potential rewrites: ${potentialRewrites.length}")

    val rewritten = potentialRewrites.map(potentialRewrite =>
      Rewrite.applyRuleAt(
        lambda = toyLambda,
        expr = potentialRewrite.expr,
        rule = potentialRewrite.rule))

    println(rewritten)


    val potentialRewrites2 = Rewrite.listAllPossibleRewritesForRules(
      lambda = rewritten.head,
      rules = Seq(FissionRules.mapFissionWithZipOutside))
    println(f"Potential rewrites 2: ${potentialRewrites2.length}")

    val rewritten2 = potentialRewrites2.map(potentialRewrite =>
      Rewrite.applyRuleAt(
        lambda = rewritten.head,
        expr = potentialRewrite.expr,
        rule = potentialRewrite.rule))

    println(rewritten2)
  }
}
