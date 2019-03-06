import lift.arithmetic.Var
import utils.GraphSort

package object exploration {
  trait ValidationRule {
    val params: Vector[Var]

    val name: String
    val comment: String

    def isValid(values: Vector[Int]): Boolean
  }

  case class ValidationRule1D(name: String,
                              comment: String,
                              param0: Var,
                              condition: (Int) => Boolean)
    extends ValidationRule {
    val params: Vector[Var] = Vector(param0)

    def isValid(values: Vector[Int]): Boolean = condition(values: _*)
  }

  case class ValidationRule2D(name: String,
                              comment: String,
                              param0: Var,
                              param1: Var,
                              condition: (Int, Int) => Boolean)
    extends ValidationRule {
    val params: Vector[Var] = Vector(param0, param1)

    def isValid(values: Vector[Int]): Boolean = condition(values: _*)

  }

  case class ValidationRule3D(name: String,
                              comment: String,
                              param0: Var,
                              param1: Var,
                              param2: Var,
                              condition: (Int, Int, Int) => Boolean)
    extends ValidationRule {
    val params: Vector[Var] = Vector(param0, param1, param2)

    def isValid(values: Vector[Int]): Boolean = condition(values: _*)
  }

  object ValidationRule {
    def apply(name: String, comment: String,
              param0: Var,
              condition: (Int) => Boolean): ValidationRule1D =
      ValidationRule1D(name, comment, param0, condition)

    def apply(name: String, comment: String,
              param0: Var, param1: Var,
              condition: (Int, Int) => Boolean): ValidationRule2D =
      ValidationRule2D(name, comment, param0, param1, condition)

    def apply(name: String, comment: String,
              param0: Var, param1: Var, param2: Var,
              condition: (Int, Int, Int) => Boolean): ValidationRule3D =
      ValidationRule3D(name, comment, param0, param1, param2, condition)
  }

  /**
    * ValidationRules contains the list of rules and a map of parameters, where with each parameter we associate
    * a list of rules that apply to the parameters. Same rule might be referenced my multiple such lists.
    * The list of rules is sorted by the number of parameters they reference
    *
    * @param rules List of unique rules
    * @param rulesPerParam A map of parameters to corresponding rules
    */
  class ValidationRules(val rules: Vector[ValidationRule],
                        val rulesPerParam: Map[Var, List[ValidationRule]]) {
    /**
      * The list of params sorted by complexity of validation. The complexity of a parameter P is defined in terms
      * of the number of other parameters that P is validated against, both directly through its immediate validation
      * rules and through the validation rules of the parameters in its immediate validation rules.
      */
    lazy val validatedParamsSortedByComplexity: List[Var] = GraphSort.topologicalSort(
      vertices = rulesPerParam.keysIterator.toVector,
      edges = rulesPerParam.flatMap(pair => {
        val param = pair._1
        val paramRules = pair._2

        paramRules.flatMap(paramRule =>
          paramRule.params.map(paramRuleParam =>
            (/*child param*/param, /*parent param*/paramRuleParam)))}).toArray)

  }

  object ValidationRules {
    def apply(rules: Vector[ValidationRule]): ValidationRules = {
      val sortedRules = rules.sortWith((rule1, rule2) => rule1.params.length <= rule2.params.length)

      new ValidationRules(sortedRules, populate(sortedRules))
    }

    /**
      * Populates the parameter rules map recursively
      */
    def populate(rulesToTraverse: Vector[ValidationRule]): Map[Var, List[ValidationRule]] = {

      def updateMap(paramRules: Map[Var, List[ValidationRule]],
                    param: Var,
                    rule: ValidationRule): Map[Var, List[ValidationRule]] = {
        if (paramRules.isEmpty || !paramRules.keySet.exists(p => p.name == param.name))
          paramRules + (param -> List(rule))
        else
          paramRules + (param -> (rule +: paramRules(param)))
      }

      rulesToTraverse match {
        case rule +: rest => rule match {

          case r@ValidationRule1D(_, _, param0, _) =>
            updateMap(populate(rest), param0, r)

          case r@ValidationRule2D(_, _, param0, param1, _) =>
            updateMap(updateMap(populate(rest), param0, r), param1, r)

          case r@ValidationRule3D(_, _, param0, param1, param2, _) =>
            updateMap(updateMap(updateMap(populate(rest), param0, r), param1, r), param2, r)

          case _ => throw new IllegalArgumentException("Unexpected rule type")
        }
        case IndexedSeq() => Map.empty[Var, List[ValidationRule]]
      }
    }
  }
}
