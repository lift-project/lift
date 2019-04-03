package exploration

import lift.arithmetic.{ArithExpr, Cst, Var}
import _root_.utils.GraphSort

class ParamConstraint(val name: String,
                      val comment: String,
                      val params: Vector[Var],
                      val lhs: ArithExpr,
                      val rhs: ArithExpr,
                      val predicate: (ArithExpr, ArithExpr) => Boolean) {
//  val params: Vector[Var] = Vector(param0)

  def isValid(values: Vector[Cst]): Boolean = {
    val substitutionTable: Map[Var, Cst] = params.zip(values).toMap

    val lhsWithValues = lhs.visitAndRebuild(ParameterRewrite.substituteVars(_, substitutionTable))
    val rhsWithValues = rhs.visitAndRebuild(ParameterRewrite.substituteVars(_, substitutionTable))

    predicate(lhsWithValues, rhsWithValues)
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

    GraphSort.topologicalSort(
      vertices = parameters.indices.toVector,
      edges = constraintsPerParam.map(pair => {
        val param = pair._1
        val paramConstraints = pair._2

        val edges = paramConstraints.flatMap(paramConstraint =>
          paramConstraint.params.filter(_ != param).map(paramConstraintParam => {

            if (parameters.indexOf(param) == -1)
              throw new IllegalArgumentException(
                s"Parameter ${param.toString} is not in the list ${parameters.map(_.toString).mkString(", ")}")

            if (parameters.indexOf(paramConstraintParam) == -1)
              throw new IllegalArgumentException(
                s"Parameter ${paramConstraintParam.toString} is not in the list ${parameters.map(_.toString).mkString(", ")}")

            ( /*child param*/ parameters.indexOf(param),
              /*parent param*/ parameters.indexOf(paramConstraintParam))
          }))
        edges}).flatten.toArray).
      map(paramIdx => parameters(paramIdx))
  }

  /**
    * An independent parameter of a constraint is one for which we do not have to check the constraint validity.
    * This is done so as to not have circular dependencies when picking random values for parameters.
    * We choose an independent parameter based on how complex is the validation of each of the parameters of
    * the constraint (i.e. how many constraints are imposed on them).
    */
//  def independentParamOfConstraint(constraint: ParamConstraint): Option[Var] =
//    if (constraint.params.length > 1) Some(constraint.params.minBy(paramsSortedByValidationComplexity.indexOf(_)))
//    // If the rule has only one parameter, then it is considered dependent (e.g. p0 % 2 == 0), i.e. we can't skip
//    // checking this constraint for this parameter
//    else None
}

object ParamConstraints {
  def apply(parameters: Vector[Var], constraints: Vector[ParamConstraint]): ParamConstraints = {
    val sortedConstraints = constraints.sortWith((constraint1, constraint2) =>
      constraint1.params.length <= constraint2.params.length)

    new ParamConstraints(parameters, sortedConstraints, findConstraintsPerParams(sortedConstraints))
  }

  /**
    * Populates the constraint map recursively
    */
  def findConstraintsPerParams(constraintsToTraverse: Vector[ParamConstraint]): Map[Var, List[ParamConstraint]] = {

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
        constraint.params.foldLeft(findConstraintsPerParams(rest)) {
          case (constraintMap: Map[Var, List[ParamConstraint]], param: Var) =>
            updateMap(constraintMap, param, constraint)
        }

      case IndexedSeq() =>
        Map.empty[Var, List[ParamConstraint]]
    }
  }
}