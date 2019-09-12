package exploration

import lift.arithmetic.{ArithExpr, Cst, SimplifiedExpr, Var}
import _root_.utils.GraphSort
import exploration.ParamConstraints.sortConstraints
import lift.arithmetic.NotEvaluableException.NotEvaluable

class ParamConstraint(val name: String,
                      val comment: String,
                      val params: Vector[Var],
                      val lhs: ArithExpr,
                      val rhs: ArithExpr,
                      val predicate: (ArithExpr with SimplifiedExpr, ArithExpr with SimplifiedExpr) => Boolean) {
//  val params: Vector[Var] = Vector(param0)

  def isValid(substitutionTable: Map[Var, Cst]): Boolean = {
    val reducedSubstitutionTable = substitutionTable.filter(pair => params.contains(pair._1))

    val lhsWithValues = lhs.visitAndRebuild(ParameterRewrite.substituteVars(_, reducedSubstitutionTable))
    val rhsWithValues = rhs.visitAndRebuild(ParameterRewrite.substituteVars(_, reducedSubstitutionTable))

    val t = predicate(lhsWithValues, rhsWithValues)
    t
  }

  def lower(substitutionTable: Map[Var, Cst]): ParamConstraint = {
    val reducedSubstitutionTable = substitutionTable.filter(pair => params.contains(pair._1))

    new ParamConstraint(
      name = name,
      comment = comment,
      params = params,
      lhs = lhs.visitAndRebuild(ParameterRewrite.substituteVars(_, reducedSubstitutionTable)),
      rhs = rhs.visitAndRebuild(ParameterRewrite.substituteVars(_, reducedSubstitutionTable)),
      predicate = predicate
    )
  }
}

/**
  * ParamConstraints contains the list of constraints and a map of parameters, where with each parameter we associate
  * a list of constraints that apply to the parameters. Same constraints might be referenced my multiple such lists.
  * The list of constraints is sorted by the number of parameters they reference
  *
  * @param constraints List of unique constraints
  * @param constraintsPerParam A map of parameters to corresponding constraints
  */
class ParamConstraints(parameters: Vector[Var],
                       val constraints: Vector[ParamConstraint],
                       val constraintsPerParam: Map[Var, List[ParamConstraint]]) {
  /**
    * The list of params sorted by complexity of validation. The complexity of a parameter P is defined in terms
    * of the number of other parameters that P is validated against, both directly through its immediate validation
    * constraints and through the constraints of the parameters in its immediate constraints.
    */
  lazy val paramsSortedByValidationComplexity: List[Var] = {
    //      val paramsToValidate = constraintsPerParam.keySet.toVector

    val paramsSortedUsingRegularConstraints = GraphSort.topologicalSort(
      vertices = parameters.indices.toVector,
      edges = ParamConstraints.collectEdges(parameters, constraintsPerParam),
      circularDependenciesAllowed = true).
      map(paramIdx => parameters(paramIdx))

    val paramsSortedUsingRangeConstraints = GraphSort.topologicalSort(
      vertices = paramsSortedUsingRegularConstraints.indices.toVector,
      edges = {
        val sortedConstraints = sortConstraints(ParameterSpace.rangeBasedConstraints(parameters))

        ParamConstraints.collectEdges(
          paramsSortedUsingRegularConstraints.toVector,
          ParamConstraints.findConstraintsPerParams(sortedConstraints, directedConstraints = true))
      },
      circularDependenciesAllowed = true).map(paramIdx => paramsSortedUsingRegularConstraints(paramIdx))


    paramsSortedUsingRangeConstraints

  }
}

object ParamConstraints {
  def apply(parameters: Vector[Var], constraints: Vector[ParamConstraint]): ParamConstraints = {
    val sortedConstraints = sortConstraints(constraints)

    new ParamConstraints(parameters, sortedConstraints,
      findConstraintsPerParams(sortedConstraints, directedConstraints = false))
  }

  def sortConstraints(constraints: Vector[ParamConstraint]) =
    constraints.sortWith((constraint1, constraint2) =>
      constraint1.params.length <= constraint2.params.length)

  /**
    * Populates the constraint map recursively
    */
  def findConstraintsPerParams(constraintsToTraverse: Vector[ParamConstraint],
                               directedConstraints: Boolean): Map[Var, List[ParamConstraint]] = {

    def updateMap(paramConstraints: Map[Var, List[ParamConstraint]],
                  param: Var,
                  constraint: ParamConstraint): Map[Var, List[ParamConstraint]] = {
      if (paramConstraints.isEmpty || !paramConstraints.keySet.exists(p => p.name == param.name))
        paramConstraints + (param -> List(constraint))
      else
        paramConstraints + (param -> (constraint +: paramConstraints(param)))
    }

    constraintsToTraverse match {
      case constraint +: rest =>
        if (directedConstraints)
          updateMap(findConstraintsPerParams(rest, directedConstraints), constraint.params.head, constraint)
        else
          constraint.params.foldLeft(findConstraintsPerParams(rest, directedConstraints)) {
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
        paramConstraint.params.filter(_ != param).map(paramConstraintParam => {

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


  def removeDuplicateConstraints(constraints: Vector[ParamConstraint]) =
  constraints.foldLeft(Vector[ParamConstraint]()) {
    case (acc, newConstraint) =>
      if (acc.exists(oldConstraint =>
        oldConstraint.lhs == newConstraint.lhs && oldConstraint.rhs == newConstraint.rhs))
        acc
      else
        acc :+ newConstraint
  }

}