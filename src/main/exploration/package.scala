package object exploration {
  class Parameter[+T](val name: String,
                      val shortName: String,
                      val range: Seq[T])

  trait ValidationRule {
    val params: Seq[Parameter[Any]]

    val name: String
    val comment: String

    def isValid(values: Seq[Any]): Boolean
  }

  case class ValidationRule1D[T0 <: Any](name: String,
                                         comment: String,
                                         param0: Parameter[T0],
                                         condition: (T0) => Boolean)
    extends ValidationRule {
    val params: Seq[Parameter[Any]] = Seq(param0)

    def isValid(values: Seq[Any]): Boolean = condition(values(0).asInstanceOf[T0])
  }

  case class ValidationRule2D[T0 <: Any, T1 <: Any](name: String,
                                                    comment: String,
                                                    param0: Parameter[T0],
                                                    param1: Parameter[T1],
                                                    condition: (T0, T1) => Boolean)
    extends ValidationRule {
    val params: Seq[Parameter[Any]] = Seq(param0, param1)

    def isValid(values: Seq[Any]): Boolean =
      condition(values(0).asInstanceOf[T0], values(1).asInstanceOf[T1])

  }

  case class ValidationRule3D[T0 <: Any, T1 <: Any, T2 <: Any](name: String,
                                                               comment: String,
                                                               param0: Parameter[T0],
                                                               param1: Parameter[T1],
                                                               param2: Parameter[T2],
                                                               condition: (T0, T1, T2) => Boolean)
    extends ValidationRule {
    val params: Seq[Parameter[Any]] = Seq(param0, param1, param2)

    def isValid(values: Seq[Any]): Boolean =
      condition(values(0).asInstanceOf[T0], values(1).asInstanceOf[T1], values(2).asInstanceOf[T2])
  }

  object ValidationRule {
    def apply[T0 <: Any](name: String, comment: String,
                         param0: Parameter[T0],
                         condition: (T0) => Boolean): ValidationRule1D[T0] =
      ValidationRule1D(name, comment, param0, condition)

    def apply[T0 <: Any, T1 <: Any](name: String, comment: String,
                                    param0: Parameter[T0], param1: Parameter[T1],
                                    condition: (T0, T1) => Boolean): ValidationRule2D[T0, T1] =
      ValidationRule2D(name, comment, param0, param1, condition)

    def apply[T0 <: Any, T1 <: Any, T2 <: Any](name: String, comment: String,
                                               param0: Parameter[T0], param1: Parameter[T1], param2: Parameter[T2],
                                               condition: (T0, T1, T2) => Boolean): ValidationRule3D[T0, T1, T2] =
      ValidationRule3D(name, comment, param0, param1, param2, condition)
  }

  /**
    * ValidationRules contains the list of rules and a map of parameters, where with each parameter we associate
    * a list of rules that apply to the parameter. Same rule might be referenced my multiple such lists.
    *
    * @param rules List of unique rules
    * @param rulesPerParam A map of parameters to corresponding rules
    */
  case class ValidationRules(rules: Seq[ValidationRule],
                             rulesPerParam: Map[Parameter[Any], Seq[ValidationRule]])

  object ValidationRules {
    def apply(rules: Seq[ValidationRule]): ValidationRules =
      new ValidationRules(rules, populate(rules))

    /**
      * Populates the parameter-rules map recursively
      */
    def populate(rulesToTraverse: Seq[ValidationRule]): Map[Parameter[Any], Seq[ValidationRule]] = {

      def updateMap(paramRules: Map[Parameter[Any], Seq[ValidationRule]],
                    param: Parameter[Any],
                    rule: ValidationRule): Map[Parameter[Any], Seq[ValidationRule]] = {
        if (paramRules.isEmpty || !paramRules.contains(param))
          paramRules + (param -> Seq(rule))
        else
          paramRules + (param -> (rule +: paramRules(param)))
      }

      rulesToTraverse match {
        case Nil => Map.empty[Parameter[Any], Seq[ValidationRule]]
        case rule :: rest => rule match {

          case r@ValidationRule1D(_, _, param0, _) =>
            updateMap(populate(rest), param0, r)

          case r@ValidationRule2D(_, _, param0, param1, _) =>
            updateMap(updateMap(populate(rest), param0, r), param1, r)

          case r@ValidationRule3D(_, _, param0, param1, param2, _) =>
            updateMap(updateMap(updateMap(populate(rest), param0, r), param1, r), param2, r)

          case _ => throw new IllegalArgumentException("Unexpected rule type")
        }
      }
    }
  }
}
