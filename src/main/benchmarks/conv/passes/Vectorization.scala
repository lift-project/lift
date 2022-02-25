package benchmarks.conv.passes

import com.typesafe.scalalogging.Logger
import exploration.constraint_solvers.{ChocoConstraintSolver, ConstraintSolver}
import ir.ast.{FPattern, FunCall, Lambda, Lambda1, Map, Slide, Split, Transpose, Unslide, UserFun, fun}
import ir.view.View
import ir.{ArrayTypeWSWC, TypeChecker}
import lift.arithmetic.{?, ContinuousRange, Cst, Var}
import opencl.executor.Compile
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir.pattern.{MapSeq, toGlobal, toLocal, toPrivate}
import opencl.ir.{Float, GlobalMemory, InferOpenCLAddressSpace, LocalMemory, OpenCLMemoryAllocator, PrivateMemory, RemoveRedundantMemory, id}
import org.junit.Assert.assertEquals
import org.junit.Test
import rewriting.Rewrite
import benchmarks.conv.ConvExploration.debugRun
import benchmarks.conv.ConvExplorationJSONSettings
import rewriting.passes.RewritingPass
import rewriting.passes.RewritingPass.RuleApplicationHelper.{exprEquals, matchedExprItself}
import rewriting.passes.RewritingPass.{RewritePassParams, RuleApplicationHelper}
import rewriting.rules.{OpenCLRules, Rule}

import scala.collection.immutable.ListMap

object Vectorization extends ConvAutoRewritePassSpaceFactory {

  override val name: String = "Vectorization"

  private val logger = Logger(this.getClass)

  private val doRestrictRulesOnAddressSpaces: Boolean = true

  val ruleApplicationHelpers: Predef.Map[Rule, RuleApplicationHelper] = Predef.Map(
    // TODO: explore other vector lens
    OpenCLRules.mapSeqVector(4) ->
      RuleApplicationHelper(1, matchedExprItself, exprEquals)
  ).map(pair =>
    if (doRestrictRulesOnAddressSpaces)
      restrictRuleOnAddressSpaces(pair._1) -> pair._2
    else pair._1 -> pair._2)

  def manualTransformationScheme(passParams: RewritePassParams): ListMap[Var, Cst] = {
    val mapTransforms = ListMap[String, Int](
      // layer 10,
//      "mapVectorize.35." -> 0,
//      "mapVectorize.113." -> 1
      // layer 8
//      "mapVectorize.236." -> 1,
//    "mapVectorize.215." -> 0
      // layer 8 no vect
//      "mapVectorize.213." -> 0,
//      "mapVectorize.246." -> 0
    )
    // The rest are sequential

    val (mapToTransformParams, _) = passParams.exprParams.values.partition(param =>
      mapTransforms.keys.exists(aKey => param.getFullName.startsWith(aKey)))

    ListMap(
      (if (debugRun)
        (mapToTransformParams.map(param =>
          mapTransforms.find(mapTransform => param.getFullName.startsWith(mapTransform._1)) match {
            case None => throw new IllegalStateException()
            case Some(mapTransform) => param.getAEVar -> Cst(mapTransform._2)
          })).toSeq
      else Seq()): _*
    )
  }

  def preprocessLambda(lambda: Lambda): Lambda = {
    // Replace all `MapSeq(to<Mem>(userFun))` to `to<Mem>(MapSeq(userFun))` to open them up for further rewriting
    val lambdaWithHoistedASCasters =
      Rewrite.applyRuleUntilCannot(lambda, Rule("MapSeq(to<Mem>(userFun)) => to<Mem>(MapSeq(userFun))", {
        case FunCall(MapSeq(
        Lambda1(mapLambdaParams, FunCall(fPattern @ FPattern(Lambda(_,
        FunCall(uf: UserFun, _*), _)), casterArg))), arg)
          if (
            fPattern.isInstanceOf[toGlobal] ||
              fPattern.isInstanceOf[toLocal] ||
              fPattern.isInstanceOf[toPrivate]) &&
            casterArg == mapLambdaParams.head =>

          fPattern.copy(MapSeq(uf)).apply(arg)
      }))

    // Infer the views required to pattern match the mapSeqVector rule
    TypeChecker(lambdaWithHoistedASCasters)
    RangesAndCounts(lambdaWithHoistedASCasters, NDRange(?), NDRange(?), collection.Map())
    InferOpenCLAddressSpace(lambdaWithHoistedASCasters)
    OpenCLMemoryAllocator(lambdaWithHoistedASCasters)
    RemoveRedundantMemory(lambdaWithHoistedASCasters)
    View(lambdaWithHoistedASCasters)

    lambdaWithHoistedASCasters
  }

  def apply(layerConfigIdx: Int,
            lambda: Lambda,
            initialSeed: Int
           )(implicit jsonSettings: ConvExplorationJSONSettings): Option[ConvAutoRewritePassSearchSpace] = {

    val preprocessedLambda = preprocessLambda(lambda)

    val rewritingPass: RewritingPass = new RewritingPass(
      ruleApplicationHelpers, paramsLabel = "mapVectorize", preprocessedLambda, rewriteWhereverPossible = true)

    val space = ConvAutoRewritePassSearchSpace(rewritingPass,
      independentParameters = manualTransformationScheme(rewritingPass.rewriteParams),
      inferredAndManualConstraints = Vector())

    space.setSolver(ConstraintSolver.Choco, initialSeed)
//    space.setSolver(ConstraintSolver.Z3, initialSeed)

    logger.info(s"$name rewrite pass search space initialized")
    Some(space)
  }

  def restrictRuleOnAddressSpaces(regularRule: Rule): Rule =
    Rule(regularRule.desc, {
      val originalRewrite = regularRule.rewrite

      // Adds new clause to the pattern
      { case e if (
        e match {
          case call @ FunCall(_, arg)
            if call.addressSpace == PrivateMemory &&
              Set(GlobalMemory, LocalMemory).contains(arg.addressSpace) => true
          case _ => false
        }) && originalRewrite.isDefinedAt(e) =>
        originalRewrite(e)
      }
    })
}

class Vectorization() {
  @Test
  def t0_slidedNaive(): Unit = {
    val N = Var("N")//10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = 3//Var("wSize", ContinuousRange(3, 7))//3
    val wStep = 1//Var("wStep", ContinuousRange(1, 3)) //1

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq( MapSeq(toGlobal(id)) ) ) o
          MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
          Split(2) o Slide(wSize, wStep) $
          input)

    val pass = new RewritingPass(Vectorization.ruleApplicationHelpers, paramsLabel = "mapVectorize",
      Vectorization.preprocessLambda(f), rewriteWhereverPossible = true)

    println(pass.rewriteParams.exprParams)
    assertEquals(2, pass.rewriteParams.exprParams.size)

    println(Compile(f))
  }

  @Test
  def t1_slidedTransposed(): Unit = {
    val N = Var("N")//10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq( MapSeq(toGlobal(id)) ) ) o
          MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
          Map(Transpose()) o Transpose() o
          Map(Split(2)) o Transpose() o Slide(wSize, wStep) $
          input)

    val pass = new RewritingPass(Vectorization.ruleApplicationHelpers, paramsLabel = "mapVectorize",
      Vectorization.preprocessLambda(f), rewriteWhereverPossible = true)

    println(pass.rewriteParams.exprParams)
    assertEquals(2, pass.rewriteParams.exprParams.size)
  }

  @Test
  def t2_slidedUnslided(): Unit = {
    val N = Var("N")//10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq(toGlobal(id)) ) o
          MapSeq( MapSeq(toGlobal(id)) o Unslide(wSize, wStep)) o
          Split(2) o Slide(wSize, wStep) $
          input)

    val pass = new RewritingPass(Vectorization.ruleApplicationHelpers, paramsLabel = "mapVectorize",
      Vectorization.preprocessLambda(f), rewriteWhereverPossible = true)

    println(pass.rewriteParams.exprParams)
    assertEquals(2, pass.rewriteParams.exprParams.size)
  }

  @Test
  def t3_slidedUnslidedSlided(): Unit = {
    val N = Var("N")//10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = 3//Var("wSize", ContinuousRange(3, 7))//3
    val wStep = 1//Var("wStep", ContinuousRange(1, 3)) //1

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq( MapSeq(toGlobal(id)) ) ) o
          MapSeq( Slide(wSize, wStep) o MapSeq(toGlobal(id)) o Unslide(wSize, wStep)) o
          Split(2) o Slide(wSize, wStep) $
          input)

    val pass = new RewritingPass(Vectorization.ruleApplicationHelpers, paramsLabel = "mapVectorize",
      Vectorization.preprocessLambda(f), rewriteWhereverPossible = true)

    println(pass.rewriteParams.exprParams)
    assertEquals(2, pass.rewriteParams.exprParams.size)
    println(Compile(f))
  }
}
