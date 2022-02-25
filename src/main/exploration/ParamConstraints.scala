package exploration

import lift.arithmetic.{ArithExpr, Cst, SimplifiedExpr, Var}
import _root_.utils.GraphSort
import exploration.constraint_solvers.RelationalExpr
import lift.arithmetic.NotEvaluableException.NotEvaluable
import org.chocosolver.solver.Model

import scala.language.implicitConversions

trait Predicate
case class ChocoPredicateStr(str: String) extends Predicate
case class ChocoPredicate(predicate: (Model, List[ArithExpr]) => Unit) extends Predicate
case class ConstraintDSLPredicate(predicate: List[ArithExpr] => RelationalExpr) extends Predicate

object PredicateWrappers {
  implicit def chocoPredicateStrWrapper(str: String): ChocoPredicateStr = ChocoPredicateStr(str)
  implicit def chocoPredicateWrapper(p: (Model, List[ArithExpr]) => Unit): ChocoPredicate = ChocoPredicate(p)
  implicit def constraintDSLPredicateWrapper(p: List[ArithExpr] => RelationalExpr
                                            ): ConstraintDSLPredicate = ConstraintDSLPredicate(p)
}

case class ParamConstraint(name: String,
                           comment: String,
                           params: List[ArithExpr],
                           arithExprPredicate: List[ArithExpr with SimplifiedExpr] => Boolean,
                           predicate: Predicate
                           // A workaround for the integer overflow problem where even if the expression result doesn't overflow, a subexpression does.
                           // Only set mightOverflow for constraints where ignoring the constraint will not break lead to invalid candidates
                           /*mightOverflow: Boolean*/) {
  val variables: Vector[Var] = params.flatMap(ArithExpr.collectVars).toVector

  def isValid(substitutionTable: Map[Var, Cst]): Boolean = {
    val reducedSubstitutionTable = substitutionTable.filter(pair => variables.contains(pair._1))

    val arguments: List[ArithExpr with SimplifiedExpr] =
      params.map(_.visitAndRebuild(ParameterRewrite.substitute(_, reducedSubstitutionTable)))

    arguments.foreach(argument =>
      if (ArithExpr.collectVars(argument).nonEmpty)
        println(f"WARNING: an argument of the constraint $name contains variables that cannot be substituted with " +
          f"values: $argument"))

    val result = arithExprPredicate(arguments)
    result
  }

  def lower(substitutionTable: Map[Var, Cst]): ParamConstraint = {
    val reducedSubstitutionTable = substitutionTable.filter(pair => variables.contains(pair._1))

    new ParamConstraint(
      name = name,
      comment = comment,
      params = params.map(_.visitAndRebuild(ParameterRewrite.substitute(_, reducedSubstitutionTable))),
      arithExprPredicate,
      predicate
    )
  }

  override def toString: String = name + ": " + params.head + " " + predicate +
    (if (params.length > 1) " " + params(1) else "")
}

object ParamConstraints {
  def sortConstraints(constraints: Vector[ParamConstraint]): Vector[ParamConstraint] = {
    if (constraints.nonEmpty) {
      constraints.sortWith((constraint1, constraint2) =>
        constraint1.variables.length < constraint2.variables.length)
    } else constraints
  }

  /**
    * Populates the constraint map recursively
    */
  def findConstraintsPerParams(constraintsToTraverse: Vector[ParamConstraint],
                               directedConstraints: Boolean): Map[Var, List[ParamConstraint]] = {

    def updateMap(paramConstraints: Map[Var, List[ParamConstraint]],
                  param: Var,
                  constraint: ParamConstraint): Map[Var, List[ParamConstraint]] = {
      if (paramConstraints.isEmpty || !paramConstraints.contains(param))
        paramConstraints + (param -> List(constraint))
      else
        paramConstraints + (param -> (constraint +: paramConstraints(param)))
    }

    constraintsToTraverse match {
      case constraint +: rest =>
        if (directedConstraints)
          updateMap(findConstraintsPerParams(rest, directedConstraints), constraint.variables.head, constraint)
        else
          constraint.variables.foldLeft(findConstraintsPerParams(rest, directedConstraints)) {
            case (constraintMap: Map[Var, List[ParamConstraint]], param: Var) =>
              updateMap(constraintMap, param, constraint)
          }

      case IndexedSeq() =>
        Map.empty[Var, List[ParamConstraint]]
    }
  }

  def lessThanOrEqual(lhs: ArithExpr, rhs: ArithExpr): Boolean = {
    lhs == rhs || (ArithExpr.isSmaller(lhs, rhs) match {
      case Some(isSmaller) => isSmaller
      case None => throw NotEvaluable
    })
  }

  def greaterThanOrEqual(lhs: ArithExpr, rhs: ArithExpr): Boolean = {
    lhs == rhs || (ArithExpr.isSmaller(rhs, lhs) match {
      case Some(isSmaller) => isSmaller
      case None => throw NotEvaluable
    })
  }

  def collectEdges(parameters: Vector[Var],
                   constraintsPerParam: Map[Var, List[ParamConstraint]]): Array[(Int, Int)] =
    constraintsPerParam.map(pair => {
      val param = pair._1
      val paramConstraints = pair._2

      val edges = paramConstraints.flatMap(paramConstraint =>
        paramConstraint.variables.filter(_ != param).map(paramConstraintParam => {

          if (parameters.indexOf(param) == -1)
            throw new IllegalArgumentException(
              s"Parameter ${param.toString} is not in the list ${parameters.map(_.toString).mkString(", ")}")

          if (parameters.indexOf(paramConstraintParam) == -1)
            throw new IllegalArgumentException(
              s"Parameter ${paramConstraintParam.toString} is not in the list " +
                s"${parameters.map(_.toString).mkString(", ")}")

          ( /*child param*/ parameters.indexOf(param),
            /*parent param*/ parameters.indexOf(paramConstraintParam))
        }))
      edges}).flatten.toArray


  /**
   * apply produces the list of constraints and a map of parameters, where with each parameter we associate
   * a list of constraints that apply to the parameters. Same constraints might be referenced by multiple such lists.
   * The list of constraints is sorted by the number of parameters they reference
   */
  def apply(parameters: Vector[Var],
            inferredAndManualConstraints: Vector[ParamConstraint]): (
    Vector[ParamConstraint], Vector[ParamConstraint], Map[Var, List[ParamConstraint]], List[Var]) = {

    val rangeBasedConstraints: Vector[ParamConstraint] = sortConstraints(ParameterSpace.rangeBasedConstraints(parameters))

    // A list of allConstraints sorted by the number of parameters per constraint
    val allConstraints: Vector[ParamConstraint] = sortConstraints(inferredAndManualConstraints ++ rangeBasedConstraints)

    val constraintsPerParam: Map[Var, List[ParamConstraint]] = ParamConstraints.findConstraintsPerParams(
      allConstraints, directedConstraints = false)
    /**
     * The list of params sorted by complexity of validation. The complexity of a parameter P is defined in terms
     * of the number of other parameters that P is validated against, both directly through its immediate validation
     * allConstraints and through the allConstraints of the parameters in its immediate allConstraints.
     */
    lazy val paramsSortedByValidationComplexity: List[Var] = { // TODO: confirm that the val is still lazy even when we pass its reference below
      //      val paramsToValidate = constraintsPerParam.keySet.toVector

      val paramsSortedUsingRegularConstraints = GraphSort.topologicalSort(
        vertices = parameters.indices.toVector,
        edges = ParamConstraints.collectEdges(parameters, constraintsPerParam),
        circularDependenciesAllowed = true).
        map(paramIdx => parameters(paramIdx))

      val paramsSortedUsingRangeConstraints = GraphSort.topologicalSort(
        vertices = paramsSortedUsingRegularConstraints.indices.toVector,
        edges = {
          ParamConstraints.collectEdges(
            paramsSortedUsingRegularConstraints.toVector,
            ParamConstraints.findConstraintsPerParams(rangeBasedConstraints, directedConstraints = true))
        },
        circularDependenciesAllowed = true).map(paramIdx => paramsSortedUsingRegularConstraints(paramIdx))


      paramsSortedUsingRangeConstraints
    }

    (allConstraints, rangeBasedConstraints, constraintsPerParam, paramsSortedByValidationComplexity)
  }
}