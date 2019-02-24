package exploration

/**
  * All information defining parameter space
  *
  * @param parameters unique parameters defining the space
  * @param validationRules the rules declaring what parameter value combinations are valid
  * @param combinations generated combinations
  * @param numberOfCombinations the number of combinations stored (might be a subset of all possible combinations)
  */
case class ParameterSpace(parameters: Seq[Parameter[Any]],
                          validationRules: ValidationRules,
                          combinations: Seq[Seq[Any]],
                          numberOfCombinations: Int)
