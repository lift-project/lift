package benchmarks.conv.passes

import com.typesafe.scalalogging.Logger
import exploration.constraint_solvers.{ChocoConstraintSolver, ConstraintSolver}
import ir.ast.{Lambda, Map, Slide, Split, Transpose, fun}
import ir.view.View
import ir.{ArrayTypeWSWC, TypeChecker}
import lift.arithmetic.{?, ContinuousRange, Cst, Var}
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir.pattern.{MapSeq, toGlobal}
import opencl.ir.{Float, InferOpenCLAddressSpace, OpenCLMemoryAllocator, RemoveRedundantMemory, id}
import org.junit.Assert.assertEquals
import org.junit.Test
import benchmarks.conv.ConvExploration.debugRun
import benchmarks.conv.ConvExplorationJSONSettings
import rewriting.macrorules
import rewriting.passes.RewritingPass
import rewriting.passes.RewritingPass.{RewritePassParams, RuleApplicationHelper}
import rewriting.passes.RewritingPass.RuleApplicationHelper.{exprEquals, matchedExprItself}
import rewriting.rules.Rule
//import rewriting.utils.NumberExpression

import scala.collection.immutable.ListMap


object Unsliding extends ConvAutoRewritePassSpaceFactory {

  override val name: String = "Unsliding"

  private val logger = Logger(this.getClass)

  val ruleApplicationHelpers: Predef.Map[Rule, RuleApplicationHelper] = Predef.Map(
    // TODO: reenable, but prevent clashes
//    macrorules.Unsliding.unslideSlide -> RuleApplicationHelper(1, matchedExprItself, exprEquals),
    macrorules.Unsliding.unslideSlide2d -> RuleApplicationHelper(2, matchedExprItself, exprEquals)
  )

  def manualTransformationScheme(passParams: RewritePassParams): ListMap[Var, Cst] = {
    val mapTransforms = ListMap[String, Int](
//      "mapUnslide.213." -> 1
    )
    // The rest are sequential

    val (mapToUnslideParams, _) = passParams.exprParams.values.partition(param =>
      mapTransforms.keys.exists(aKey => param.getFullName.startsWith(aKey)))

    ListMap(
      (if (debugRun)
        (mapToUnslideParams.map(param =>
          mapTransforms.find(mapTransform => param.getFullName.startsWith(mapTransform._1)) match {
            case None => throw new IllegalStateException()
            case Some(mapTransform) => param.getAEVar -> Cst(mapTransform._2)
          })).toSeq
      else Seq()): _*
    )
  }

  def preprocessLambda(lambda: Lambda): Lambda = {

    // Infer the views required to pattern match the unslideSlide rule
    TypeChecker(lambda)
    RangesAndCounts(lambda, NDRange(?), NDRange(?), collection.Map())
    InferOpenCLAddressSpace(lambda)
    OpenCLMemoryAllocator(lambda)
    RemoveRedundantMemory(lambda)
    View(lambda)

    lambda
  }

  def apply(layerConfigIdx: Int,
            lambda: Lambda,
            initialSeed: Int
           )(implicit jsonSettings: ConvExplorationJSONSettings): Option[ConvAutoRewritePassSearchSpace] = {

    val preprocessedLambda = preprocessLambda(lambda)
//    globalNumbering = NumberExpression.breadthFirst(preprocessedLambda.body)
    val rewritingPass: RewritingPass = new RewritingPass(
      // Made rewriteWhereverPossible false since unsliding eliminates one map, removing a parallelization target.
      ruleApplicationHelpers, paramsLabel = "mapUnslide", preprocessedLambda, rewriteWhereverPossible = false)

    val space = ConvAutoRewritePassSearchSpace(rewritingPass,
      independentParameters = manualTransformationScheme(rewritingPass.rewriteParams),
      inferredAndManualConstraints = Vector())

    space.setSolver(ConstraintSolver.Choco, initialSeed)
//    space.setSolver(ConstraintSolver.Z3, initialSeed)

    logger.info(s"$name rewrite pass search space initialized")
    Some(space)
  }
}

class Unsliding() {

  @Test
  def t0_slidedNaive(): Unit = {
    val N = Var("N")//10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq(     MapSeq(toGlobal(id)) /*here*/) ) o
          MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
          Split(2) o Slide(wSize, wStep) $
          input)

    compilePartially(f)

    val pass = new RewritingPass(
      Unsliding.ruleApplicationHelpers, paramsLabel = "mapUnslideSlide", f, rewriteWhereverPossible = false)
    println(pass.rewriteParams.exprParams)
    assertEquals(1, pass.rewriteParams.exprParams.size)
  }

  @Test
  def t1_slidedTransposed(): Unit = {
    val N = Var("N")//10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq(     MapSeq(toGlobal(id)) /*here*/) ) o
          MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
          Map(Transpose()) o Transpose() o
          Map(Split(2)) o Transpose() o Slide(wSize, wStep) $
          input)

    compilePartially(f)

    val pass = new RewritingPass(
      Unsliding.ruleApplicationHelpers, paramsLabel = "mapUnslideSlide", f, rewriteWhereverPossible = false)
    println(pass.rewriteParams.exprParams)
    assertEquals(1, pass.rewriteParams.exprParams.size)
  }

  private def compilePartially(lambda: Lambda): Unit = {
    TypeChecker(lambda)
    RangesAndCounts(lambda, NDRange(?), NDRange(?), collection.Map())
    InferOpenCLAddressSpace(lambda)
    OpenCLMemoryAllocator(lambda)
    RemoveRedundantMemory(lambda)
    View(lambda)
  }
}
