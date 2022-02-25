package benchmarks.conv.passes

import benchmarks.conv.ConvExplorationJSONSettings
import com.typesafe.scalalogging.Logger
import exploration.ParamConstraint
import ir.ast.Lambda
import lift.arithmetic.{Cst, Var}
import benchmarks.conv.ConvExplorationJSONSettings
import benchmarks.conv.ConvExplorationJSONSettings
import rewriting.passes.RewritingPass.{RewritePassParams, RuleApplicationHelper}
import rewriting.rules.Rule

import scala.collection.immutable.ListMap

object Coalescing extends ConvAutoRewritePassSpaceFactory {

  override val name: String = "Coalescing"

  private val logger = Logger(this.getClass)

  /**
   * Numeric representation of transformations for the constraint solver
   */
  //noinspection TypeAnnotation
  object Code extends Enumeration {

    protected case class Val(value: Int) extends super[Enumeration].Val {
      def apply(): Int = value
    }

    val doNothing = Val(0)
    val coalesceAndScatter = Val(1)
    val coalesceAndGather = Val(2)
  }

  val ruleApplicationHelpers: Predef.Map[Rule, RuleApplicationHelper] = Predef.Map(
//    Rule("Do nothing", { case e => e }) -> RuleApplicationHelper(doNothing(), matchedExprItself, exprEquals),
//
//    null -> RuleApplicationHelper(coalesceAndScatter(), matchedExprItself, exprEquals),
//    null -> RuleApplicationHelper(coalesceAndGather(), matchedExprItself, exprEquals)
  )

  def coalescingRules(context: ASTContext,
                      passParams: RewritePassParams): Vector[ParamConstraint] = {
    implicit val c: ASTContext = context

    Vector()
  }

  def heuristics(context: ASTContext,
                 passParams: RewritePassParams): Vector[ParamConstraint] = {
    implicit val c: ASTContext = context

    Vector()
  }

  def debuggingConstraints(context: ASTContext,
                           passParams: RewritePassParams): Vector[ParamConstraint] = {
    implicit val c: ASTContext = context

    Vector()
  }

  def manualTransformationScheme(passParams: RewritePassParams): ListMap[Var, Cst] = {
    val mapTransforms = ListMap[String, Int](
      // layer 3
//      "mapTransform.24." -> (10*Code.mapWrg() + Code.parDims(2)()),
//      "mapTransform.26." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.30." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.31." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.32." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.33." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.104." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.110." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.138." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.139." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.140." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.142." -> (Code.replaceInnerMapWithOuter())
    )

    val (parMapParams, seqMapParams) = passParams.exprParams.values.partition(param =>
      mapTransforms.keys.exists(aKey => param.getFullName.startsWith(aKey)))

    ListMap(
      (if (false)
        (parMapParams.map(param =>
          mapTransforms.find(mapTransform => param.getFullName.startsWith(mapTransform._1)) match {
            case None => throw new IllegalStateException()
            case Some(mapTransform) => param.getAEVar -> Cst(mapTransform._2)
          })
          //++ seqMapParams.map(param => param.getAEVar -> Cst(Code.mapSeq()))
          ).toSeq
      else Seq()): _*
    )
  }

  def preprocessLambda(lambda: Lambda): Lambda = {
    throw new NotImplementedError()
  }

  def apply(layerConfigIdx: Int,
            lambda: Lambda,
            initialSeed: Int)(implicit jsonSettings: ConvExplorationJSONSettings): Option[ConvAutoRewritePassSearchSpace] = {
    throw new NotImplementedError()
  }
}
