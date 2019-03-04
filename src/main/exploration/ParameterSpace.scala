package exploration

import lift.arithmetic.{Cst, Var}

/**
  * All information defining parameter space
  *
  * @param parameters unique parameters defining the space
  * @param validationRules the rules declaring what parameter value combinations are valid
  */
class ParameterSpace(val parameters: Vector[Var],
                     val validationRules: ValidationRules) {
}
