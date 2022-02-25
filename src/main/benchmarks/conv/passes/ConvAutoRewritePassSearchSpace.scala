package benchmarks.conv.passes

import benchmarks.conv.ConvExplorationJSONSettings
import com.typesafe.scalalogging.Logger
import exploration.constraint_solvers.ConstraintSolver.SolutionNotFound
import exploration.{ParamConstraint, ParameterSpace}
import ir.TypeChecker
import ir.ast.{AbstractMap, Expr, FPattern, FunCall, Lambda}
import lift.arithmetic.{Cst, Var}
import opencl.ir.{InferOpenCLAddressSpace, LocalMemory, PrivateMemory, UndefAddressSpace}
import benchmarks.conv.exec
import rewriting.passes.RewritingPass
import rewriting.passes.RewritingPass.RewritePassParams
import rewriting.utils.Utils

import scala.collection.immutable.{ListMap, ListSet}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

case class ConvAutoRewritePassSearchSpace private (rewritingPass: RewritingPass,
                                                   override val independentParameters: ListMap[Var, Cst] = ListMap(),
                                                   override val inferredAndManualConstraints: Vector[ParamConstraint])
  extends ParameterSpace(independentParameters,
    rewritingPass.rewriteParams.paramList.map(_.getAEVar).toVector.diff(independentParameters.keys.toSeq),
    inferredAndManualConstraints) {

  def getNextRandomPoint(tunePointIdx: Int,
                         enforceSolutionUniqueness: Boolean,
                         timeout: Int): (RewritePassParams # RewriteParamValuesT, ListMap[Var, Cst]) = {
    try {
      val encodedValues = try {
        Await.result(
          Future {
            getSolver.getNextRandomPoint(
              enforceSolutionUniqueness,
              parameterTypeName = "rewrite pass",
              solutionIdx = Some(tunePointIdx),
              returnIndependentParamArgsToo = true)},
          timeout seconds)
      } catch {
        case e: TimeoutException =>
          //noinspection ScalaDeprecation
          exec.lastThread.getOrElse(throw new RuntimeException("Not started")).stop()
          throw ConvAutoRewritePassSearchSpaceExhausted(s"Could not find rewrite pass param values in $timeout seconds. " +
            "Rewrite param search space exhausted.")
      }

      val encodedValuesSorted = rewritingPass.rewriteParams.params.map(param => encodedValues(param.getAEVar))

      (rewritingPass.rewriteParams.decode(Some(encodedValuesSorted)), encodedValues)
    } catch {
      case e: SolutionNotFound =>
        throw ConvAutoRewritePassSearchSpaceExhausted("Conv auto rewrite pass search space exhausted: " + e.msg)
    }
  }
}

case class ConvAutoRewritePassSearchSpaceExhausted(msg: String) extends Exception(msg)

object ConvAutoRewritePassSpaceFactory {
  val orderedPasses: List[ConvAutoRewritePassSpaceFactory] =
    List(/*Unsliding, */ParallelizationAndFusion, Vectorization)

  def nextRewritePass(performedPasses: mutable.ListMap[ConvAutoRewritePassSpaceFactory, RewritePassParams # RewriteParamValuesT]
                     ): ConvAutoRewritePassSpaceFactory = {
    assert(performedPasses.size < orderedPasses.length)
    orderedPasses(performedPasses.size)
  }
}

abstract class ConvAutoRewritePassSpaceFactory() {
  val name: String
  private val logger = Logger(this.getClass)

  class ASTContext(val lambda: Lambda,
                   val rewritingPass: RewritingPass) {

    // For each Map, collects concrete nested Maps
    val allNestedMaps: Predef.Map[Expr, Seq[Expr]] = {
      val childrenOf = collection.mutable.Map[Expr, Seq[Expr]]()

      rewritingPass.rewriteParams.exprParams.keys.foreach { parent: Expr =>
        if (!parent.isConcrete(visitArgs = false))
          Seq()
        else {

          val potentialChildren = rewritingPass.rewriteParams.exprParams.keys.filter(potentialChild =>
            parent != potentialChild &&
              !childrenOf.getOrElse(potentialChild, Seq()).contains(parent) &&
              potentialChild.isConcrete(visitArgs = false))

          val confirmedChildren = potentialChildren.filter(potentialChild =>
            Utils.getExprForPatternInANest(parent, {
              case e if e == potentialChild =>
            }).isDefined).toSeq

          if (confirmedChildren.nonEmpty)
            childrenOf(parent) = confirmedChildren
        }
      }

      childrenOf.toMap
    }

    val allOuterMaps: Predef.Map[Expr, Seq[Expr]] = {
      val parentsOf = collection.mutable.Map[Expr, Seq[Expr]]()

      allNestedMaps.foreach { case (parentMap, innerMaps) => innerMaps.foreach(innerMap => {
        parentsOf(innerMap) = parentsOf.get(innerMap) match {
          case Some(otherParents) => otherParents :+ parentMap
          case None => Seq(parentMap)
        }
      })}

      parentsOf.toMap
    }

    implicit class ExprWithRelationshipsToOtherMaps(mapExpr: Expr) {
      implicit def isConcreteAncestorOf(anotherMap: Expr): Boolean = allNestedMaps.contains(mapExpr) &&
        allNestedMaps(mapExpr).contains(anotherMap)

      def hasConcreteAncestors: Boolean = allOuterMaps.contains(mapExpr)
      def hasConcreteDescendants: Boolean = allNestedMaps.contains(mapExpr)

      def immediateConcreteParent: Option[Expr] = {
        // immediate parents = such ancestors of thisMap that are not ancestors of any other ancestors of thisMap

        if (!allOuterMaps.contains(mapExpr) || allOuterMaps(mapExpr).isEmpty)
          None
        else {
          val immediateParents = allOuterMaps(mapExpr).filter(ancestor =>
            !allOuterMaps(mapExpr).exists(anotherAncestor => ancestor isConcreteAncestorOf anotherAncestor))

          if (immediateParents.isEmpty)
            throw new IllegalStateException("Circular dependency detected in map ancestry!")
          if (immediateParents.size > 1)
            throw new IllegalStateException("Found a map with more than one immediate nesting map, i.e. these ancestors " +
              "don't nest each other. This shouldn't be possible.")

          Some(immediateParents.head)
        }
      }
    }

    /**
     * Maps in an expression can be represented as a tree where each map is a node, and all other IR members are ignored.
     * Map A is an immediate child of map B if map A is nested in map B. Maps A and C are siblings if they are composed;
     * since all other IR members are ignored, the following maps would be siblings: "FPattern(mapA) o mapB" and
     * "mapA o FPattern(mapB)", e.g. "toGlobal(mapA) o mapB".
     * A nesting chain is a sequence of nested maps that starts in the root of the map tree and ends in one of the leaves;
     * there are as many nesting chains as there are leaves.
     * Nesting chains are useful for constraints that are too specific to be built using such global relations
     * as childrenOf and siblingsOf.
     */
    val nestingChains: ListSet[Seq[Expr]] = {
      // Find leaves, i.e. maps that don't have any inner maps
      val innermostMaps = allOuterMaps.keys.filter(innerMap => !(innerMap hasConcreteDescendants))

      ListSet(innermostMaps.map(innermostMap => {
        var currentMap = innermostMap
        val nestingChain: ListBuffer[Expr] = ListBuffer(currentMap)

        val traversedMaps: mutable.Set[Expr] = mutable.Set()

        while (currentMap hasConcreteAncestors) {
          val parent = currentMap.immediateConcreteParent match {
            case Some(map) => map
            case None => throw new IllegalStateException("Current map has an ancestor, but couldn't find an " +
              "immediate parent. Something went wrong.")
          }

          nestingChain.prepend(parent)
          currentMap = parent

          // Safety check
          if (traversedMaps.contains(currentMap))
            throw new IllegalStateException("Circular dependency detected in map ancestry!")
          else traversedMaps += currentMap
        }

        nestingChain.toList
      }).toSeq: _*)
    }

//    val immediateOuterMap: Predef.Map[Expr, Option[Expr]] = Predef.Map(rewritingPass.rewriteParams.exprParams.keys.map(mapExpr =>
//      mapExpr -> (nestingChains.find(_.contains(mapExpr)) match {
//        case None => throw new IllegalStateException("Found a map that's not in any nesting chains")
//        case Some(nestingChain) =>
//          val mapIdxInChain = nestingChain.indexOf(mapExpr)
//          if (mapIdxInChain == -1)
//            throw new IllegalStateException()
//          else if (mapIdxInChain == 0) None
//          else Some(nestingChain(mapIdxInChain - 1))
//      })
//    ).toSeq: _*)

    val immediateNestedMap: Predef.Map[Expr, Option[Expr]] =
      Predef.Map(rewritingPass.rewriteParams.exprParams.keys.map(mapExpr =>
        mapExpr -> (nestingChains.find(_.contains(mapExpr)) match {
          case None => None //throw new IllegalStateException(s"Found a map that's not in any nesting chains: $mapExpr")
          case Some(nestingChain) =>
            val mapIdxInChain = nestingChain.indexOf(mapExpr)
            if (mapIdxInChain == -1)
              throw new IllegalStateException()
            else if (mapIdxInChain == nestingChain.length - 1) None
            else Some(nestingChain(mapIdxInChain + 1))
        })
      ).toSeq: _*)

    def arePerfectlyNestedMaps(outerMap: Expr, innerMap: Expr): Boolean = {
      outerMap match {
        case FunCall(AbstractMap(
        Lambda(Array(outerMapParam),
        innerCall @ FunCall(AbstractMap(_), innerCallArg), _)), _) =>

          innerCall.equals(innerMap) && innerCallArg.equals(outerMapParam)
        case _ => false
      }
    }

    /**
     * E.g. [a, b, c, d, e] (where a, b and c are perfectly nested)
     * returns [b, c]
     */
    def getPerfectlyNestedChildChain(mapNestingChain: Seq[Expr]): Seq[Expr] =
      mapNestingChain match {
        case Nil => Seq()
        case _ :: Nil => Seq()
        case m :: remaining if arePerfectlyNestedMaps(m, remaining.head) =>
          remaining.head +: getPerfectlyNestedChildChain(remaining)
        case _ => Seq()
      }


    // For each Map, determines the index of its parameter value in the list of all parameter values
    val indexOf: Predef.Map[Expr, Int] =
      Predef.Map(rewritingPass.rewriteParams.exprParams.keys.map(map =>
        map -> rewritingPass.rewriteParams.paramList.indexWhere(
          _.getAEVar == rewritingPass.rewriteParams.exprParams(map).getAEVar)).toSeq: _*)

    val localMemUsed: Boolean = Expr.visitWithState(false)(lambda.body, {
      case (e, false) if e.addressSpace.containsAddressSpace(LocalMemory) => true
      case (_, localMemDetected) => localMemDetected
    })
  }

  def preprocessLambda(lambda: Lambda): Lambda

  def apply(layerConfigIdx: Int,
            lambda: Lambda,
            initialSeed: Int
           )(implicit jsonSettings: ConvExplorationJSONSettings): Option[ConvAutoRewritePassSearchSpace]
}
