package exploration

/**
  * All information defining parameter space
  *
  * @param parameters unique parameters defining the space
  * @param validationRules the rules declaring what parameter value combinations are valid
  * @param combinations generated combinations
  * @param numberOfCombinations the number of combinations stored (might be a subset of all possible combinations)
  */
class ParameterSpace(val parameters: Seq[IndependentParameter[Any]],
                     val validationRules: ValidationRules,
                     val combinations: Seq[Seq[Any]],
                     val numberOfCombinations: Int)
