import lift.arithmetic.Var
import utils.GraphSort

package object exploration {
  trait ParamConstraint {
    val params: Vector[Var]

    val name: String
    val comment: String

    def isValid(values: Vector[Int]): Boolean
  }

  case class ParamConstraint1D(name: String,
                               comment: String,
                               param0: Var,
                               condition: (Int) => Boolean)
    extends ParamConstraint {
    val params: Vector[Var] = Vector(param0)

    def isValid(values: Vector[Int]): Boolean = condition(values(0))
  }

  case class ParamConstraint2D(name: String,
                               comment: String,
                               param0: Var,
                               param1: Var,
                               condition: (Int, Int) => Boolean)
    extends ParamConstraint {
    val params: Vector[Var] = Vector(param0, param1)

    def isValid(values: Vector[Int]): Boolean = condition(values(0), values(1))

  }

  case class ParamConstraint3D(name: String,
                               comment: String,
                               param0: Var,
                               param1: Var,
                               param2: Var,
                               condition: (Int, Int, Int) => Boolean)
    extends ParamConstraint {
    val params: Vector[Var] = Vector(param0, param1, param2)

    def isValid(values: Vector[Int]): Boolean = condition(values(0), values(1), values(2))
  }

  object ParamConstraint {
    def apply(name: String, comment: String,
              param0: Var,
              condition: (Int) => Boolean): ParamConstraint1D =
      ParamConstraint1D(name, comment, param0, condition)

    def apply(name: String, comment: String,
              param0: Var, param1: Var,
              condition: (Int, Int) => Boolean): ParamConstraint2D =
      ParamConstraint2D(name, comment, param0, param1, condition)

    def apply(name: String, comment: String,
              param0: Var, param1: Var, param2: Var,
              condition: (Int, Int, Int) => Boolean): ParamConstraint3D =
      ParamConstraint3D(name, comment, param0, param1, param2, condition)
  }

  /**
    * ParamConstraints contains the list of rules and a map of parameters, where with each parameter we associate
    * a list of rules that apply to the parameters. Same rule might be referenced my multiple such lists.
    * The list of rules is sorted by the number of parameters they reference
    *
    * @param constraints List of unique constraints
    * @param constraintsPerParam A map of parameters to corresponding rules
    */
  class ParamConstraints(val constraints: Vector[ParamConstraint],
                         val constraintsPerParam: Map[Var, List[ParamConstraint]]) {
    /**
      * The list of params sorted by complexity of validation. The complexity of a parameter P is defined in terms
      * of the number of other parameters that P is validated against, both directly through its immediate validation
      * rules and through the constraints of the parameters in its immediate constraints.
      */
    lazy val validatedParamsSortedByComplexity: List[Var] = {
      val paramsToValidate = constraintsPerParam.keySet.toVector

      GraphSort.topologicalSort(
        vertices = paramsToValidate.indices.toVector,
        edges = constraintsPerParam.flatMap(pair => {
          val param = pair._1
          val paramRules = pair._2

          paramRules.flatMap(paramRule =>
            paramRule.params.map(paramRuleParam =>
              (/*child param*/paramsToValidate.indexOf(param),
                /*parent param*/paramsToValidate.indexOf(paramRuleParam))))}).toArray).
        map(paramIdx => paramsToValidate(paramIdx))
    }
  }

  object ParamConstraints {
    def apply(rules: Vector[ParamConstraint]): ParamConstraints = {
      val sortedRules = rules.sortWith((rule1, rule2) => rule1.params.length <= rule2.params.length)

      new ParamConstraints(sortedRules, populate(sortedRules))
    }

    /**
      * Populates the parameter rules map recursively
      */
    def populate(rulesToTraverse: Vector[ParamConstraint]): Map[Var, List[ParamConstraint]] = {

      def updateMap(paramRules: Map[Var, List[ParamConstraint]],
                    param: Var,
                    rule: ParamConstraint): Map[Var, List[ParamConstraint]] = {
        if (paramRules.isEmpty || !paramRules.keySet.exists(p => p.name == param.name))
          paramRules + (param -> List(rule))
        else
          paramRules + (param -> (rule +: paramRules(param)))
      }

      rulesToTraverse match {
        case rule +: rest => rule match {

          case r@ParamConstraint1D(_, _, param0, _) =>
            updateMap(populate(rest), param0, r)

          case r@ParamConstraint2D(_, _, param0, param1, _) =>
            updateMap(updateMap(populate(rest), param0, r), param1, r)

          case r@ParamConstraint3D(_, _, param0, param1, param2, _) =>
            updateMap(updateMap(updateMap(populate(rest), param0, r), param1, r), param2, r)

          case _ => throw new IllegalArgumentException("Unexpected rule type")
        }
        case IndexedSeq() => Map.empty[Var, List[ParamConstraint]]
      }
    }
  }
}
