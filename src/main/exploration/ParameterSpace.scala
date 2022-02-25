package exploration

import _root_.utils.RangeValueGenerator
import exploration.ParamConstraints.lessThanOrEqual
import exploration.constraint_solvers.{ChocoConstraintSolver, ConstraintSolver}
import exploration.PredicateWrappers._
import lift.arithmetic.{ArithExpr, Cst, SimplifiedExpr, Var}
import rewriting.RewriteParamWithOrdinalValues.EnumeratedVar

import scala.collection.immutable.ListMap
import scala.util.Random

/**
  * All information defining parameter space
  *
  */
class ParameterSpace(val independentParameters: ListMap[Var, Cst],
                     val dependentParameters: Vector[Var],
                     val inferredAndManualConstraints: Vector[ParamConstraint]) {

  private def parameters: Vector[Var] = independentParameters.keys.toVector ++ dependentParameters

  val (
    constraints: Vector[ParamConstraint],
    rangeBasedConstraints: Vector[ParamConstraint],
    constraintsPerParam: Map[Var, List[ParamConstraint]],
    paramsSortedByValidationComplexity: List[Var]) =
    ParamConstraints(parameters, inferredAndManualConstraints)

  private var solver: Option[ConstraintSolver] = None

  def getSolver: ConstraintSolver = solver match {
    case None => throw new IllegalStateException()
    case Some(s) => s
  }

  def setSolver(s: ConstraintSolver.Library,
                initialSeed: Int): Unit =
    solver = Some(s match {
      case ConstraintSolver.Choco   => new ChocoConstraintSolver(this, initialSeed)
    })
}

object ParameterSpace {
  final case class EmptySpace(val message: String = "",
                              private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  def getRandomValueInRange(param: Var, random: Random, loweredRange: lift.arithmetic.Range, rangeSize: Int): Cst = {
    val randomIndexInRange = random.nextInt(rangeSize)

    param match {
      case enumVar: EnumeratedVar =>
        assert(randomIndexInRange < enumVar.enumValues.size)
        Cst(enumVar.enumValues.toSeq(randomIndexInRange))

      case _ =>
        RangeValueGenerator.generateSingleValue(loweredRange, randomIndexInRange, Some(rangeSize))
    }
  }


  def rangeBasedConstraints(params: Vector[Var]): Vector[ParamConstraint] = params.flatMap(param => {
    val minVars = ArithExpr.collectVars(param.range.min)

    val minConstraints: Vector[ParamConstraint] = if (minVars.nonEmpty)
      Vector(ParamConstraint(
        s"minValueOf${param.name}",
        f"Based on range of ${param.name} (${param.range}), the parameter value should be no less than " +
          f"${param.range.min}",
        params = List(param.range.min, param),
        arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
        predicate = "<="
      ))
    else Vector()

    val maxVars = ArithExpr.collectVars(param.range.max)

    val maxConstraints: Vector[ParamConstraint] = if (maxVars.nonEmpty)
      Vector(ParamConstraint(
        s"maxValueOf${param.name}",
        f"Based on range of ${param.name} (${param.range}), the parameter value should be no greater than " +
          f"${param.range.max}",
        params = List(param, param.range.max),
        arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
        predicate = "<="
      ))
    else Vector()


    minConstraints ++ maxConstraints
  })
}
