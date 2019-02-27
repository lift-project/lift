package exploration

object ParameterSpaceGenerator {

  /**
    * Combination counter
    * @param total total number of combinations processed so far
    * @param skip number of combinations to skip in generating
    * @param limit number of combinations to generate
    */
  private case class CombinationCounter(var total: Int, skip: Option[Int], limit: Option[Int])

  /**
    * Parameter space constructor that generates all valid parameter value combinations
    * recursively in a depth-first search. Parameters skipFirstNCombinations and maxCombinations
    * can be used to only generate a subset of the parameter space. The parameter space size can be
    * acquired without generating combination using the flag countOnly.
    *
    * @param parameters unique parameters defining the space
    * @param boundingParams a set of unique parameters (and corresponding values) that need no exploring, but might be
    *                       referred to by the validation rules. One example is in optimising neural nets: when
    *                       exploring optimisational parameters, the bounding parameters are the network
    *                       configuration parameters.
    * @param validationRules the rules declaring what parameter value combinations are valid
    * @param countOnly a flag specifying whether we should just generate combinations or just count all
    *                  possible combinations without using memory
    * @param skipFirstNCombinations a number of combinations to skip. Ignored if countOnly is set
    * @param maxCombinations a maximum number of combinations to generate. Ignored if countOnly is set
    * @return the parameter space with a generated set of combinations and the number of generated combinations, or
    *         no combinations and the number of all possible combinations if countOnly is set
    */
  def apply(parameters: Seq[Parameter[Any]],
            boundingParams: Option[Map[Parameter[Any], Any]],
            validationRules: ValidationRules,
            countOnly: Boolean,
            skipFirstNCombinations: Option[Int],
            maxCombinations: Option[Int]): ParameterSpace = {
    val counter = CombinationCounter(0, skipFirstNCombinations, maxCombinations)
    val emptyCombination = Vector[Any]()
    val emptyParamsInCombination = Map[Parameter[Any], Int]()

    val combinations = combineParameters(
      emptyParamsInCombination,
      emptyCombination,
      parameters.head,
      filterRange(
        parameters.head,
        emptyCombination,
        emptyParamsInCombination + (parameters.head -> 0),
        boundingParams, validationRules),
      parameters.tail,
      boundingParams,
      validationRules,
      counter,
      countOnly)

    ParameterSpace(parameters, validationRules, combinations,
      counter.total - {if (countOnly) 0 else counter.skip.getOrElse(0)})
  }

  /**
    * Recursive parameter combinator. Traverses a tree of parameter values in a depth-first search
    */
  private def combineParameters(paramsInCombination: Map[Parameter[Any], Int],
                                combination: Vector[Any],
                                currentParam: Parameter[Any],
                                remainingCurrentParamValues: Vector[Any],
                                remainingParams: Seq[Parameter[Any]],
                                boundingParams: Option[Map[Parameter[Any], Any]],
                                validationRules: ValidationRules,
                                counter: CombinationCounter,
                                countOnly: Boolean): Vector[Vector[Any]] = {
    def combineWithNextValueOfCurrentParam(currentParamValuesTail: Vector[Any]): Vector[Vector[Any]] =
      combineParameters(
        // The next value of this parameter is not yet in combination, so for the next iteration the list
        // of parameters in combination is the same
        paramsInCombination = paramsInCombination,
        combination = combination, // Same incomplete combination
        currentParam = currentParam, // Same parameter, more values to processes
        remainingCurrentParamValues = currentParamValuesTail, // Remaining values to process
        remainingParams = remainingParams, // No more parameters to process
        boundingParams, validationRules, counter, countOnly)

    remainingParams match {
      case Nil =>
        remainingCurrentParamValues match {
          case IndexedSeq() =>
            // No more values to process for this parameter
            Vector.empty

          case currentValue +: currentParamValuesTail =>
            /* More values, but no more parameters to process*/
            // Generate combinations using the next value of the current (and last) parameter in the combination
            finishCombinationAndProcessNextValue(
              paramsInCombination,
              combination,
              currentParam,
              currentValue,
              currentParamValuesTail,
              combineWithNextValueOfCurrentParam,
              validationRules, counter, countOnly)
        }

      case nextParam :: remainingParamsTail =>
        remainingCurrentParamValues match {
          case IndexedSeq() =>
            // No more values to process for this parameter
            Vector.empty

          case currentValue +: currentParamValuesTail =>
            /* More values and parameters to process */
            // Depth-first search: with the current value added to the combination, generate all combinations using
            // the remaining parameters, then generate combinations using the next value of the current parameter in
            // the combination
            finishCombinationAndProcessNextValueAndParameters(
              paramsInCombination,
              combination,
              currentParam,
              currentValue,
              currentParamValuesTail,
              nextParam,
              remainingParamsTail,
              combineWithNextValueOfCurrentParam,
              boundingParams, validationRules, counter, countOnly)
        }
    }
  }

  /**
    * Helper function for generating an actual combination at tree leaves, when no parameters are left to explore
    */
  private def finishCombinationAndProcessNextValue(paramsInCombination: Map[Parameter[Any], Int],
                                                   combination: Vector[Any],
                                                   currentParam: Parameter[Any],
                                                   currentValue: Any,
                                                   currentParamValuesTail: Vector[Any],
                                                   combineWithNextValueOfCurrentParam: Vector[Any] => Vector[Vector[Any]],
                                                   validationRules: ValidationRules,
                                                   counter: CombinationCounter,
                                                   countOnly: Boolean): Vector[Vector[Any]] = {
    if (!countOnly) {
      if (counter.limit.isEmpty || counter.total < (counter.limit.get + counter.skip.getOrElse(0))) {
        counter.total += 1
        if (counter.skip.isEmpty || counter.total > counter.skip.get) {
          // Finish the combination and generate the next combination
          Vector(combination :+ currentValue) ++
            combineWithNextValueOfCurrentParam(currentParamValuesTail)
        } else {
          // Skip combination
          combineWithNextValueOfCurrentParam(currentParamValuesTail)
        }
      } else // Reached the limit of combinations that we are allowed to generate. Stop
        Vector.empty
    } else {
      // Do not create a combination, just continue counting
      counter.total += 1
      combineWithNextValueOfCurrentParam(currentParamValuesTail)
    }
  }

  /**
    * Helper function for updating an accumulated combination while traversing the tree, i.e. when more parameters are
    * yet to be processed
    */
  private def finishCombinationAndProcessNextValueAndParameters(paramsInCombination: Map[Parameter[Any], Int],
                                                                combination: Vector[Any],
                                                                currentParam: Parameter[Any],
                                                                currentValue: Any,
                                                                currentParamValuesTail: Vector[Any],
                                                                nextParam: Parameter[Any],
                                                                remainingParamsTail: Seq[Parameter[Any]],
                                                                combineWithNextValueOfCurrentParam: Vector[Any] => Vector[Vector[Any]],
                                                                boundingParams: Option[Map[Parameter[Any], Any]],
                                                                validationRules: ValidationRules,
                                                                counter: CombinationCounter,
                                                                countOnly: Boolean): Vector[Vector[Any]] = {
    // The combination now contains current parameter value, so add it to the list of params in combination
    val updatedParamsInCombination = paramsInCombination + (currentParam -> paramsInCombination.size)
    // Update combination
    val updatedCombination = combination :+ currentValue

    if (!countOnly) {
      if (counter.limit.isEmpty || counter.total < (counter.limit.get + counter.skip.getOrElse(0))) {
        // Current value, other parameter
        combineParameters(
          paramsInCombination = updatedParamsInCombination,
          combination = updatedCombination,
          currentParam = nextParam, // Same parameter, more values to processes
          remainingCurrentParamValues = // All the filtered values of the next parameter
            filterRange(nextParam, updatedCombination,
              updatedParamsInCombination + (nextParam -> updatedParamsInCombination.size),
              boundingParams, validationRules),
          remainingParams = remainingParamsTail, // Remaining parameters to process
          boundingParams, validationRules, counter, countOnly) ++
          // Next value, same parameter
          combineWithNextValueOfCurrentParam(currentParamValuesTail)
      } else // Reached the limit of combinations that we are allowed to generate. Stop
        Vector.empty

    } else {
      // Do not create a combination, just continue counting
      combineParameters(
        paramsInCombination = updatedParamsInCombination,
        combination = updatedCombination,
        currentParam = nextParam, // Same parameter, more values to processes
        remainingCurrentParamValues = // All the filtered values of the next parameter
          filterRange(nextParam, updatedCombination,
            updatedParamsInCombination + (nextParam -> updatedParamsInCombination.size),
            boundingParams, validationRules),
        remainingParams = remainingParamsTail, // Remaining parameters to process
        boundingParams, validationRules, counter, countOnly) ++ combineWithNextValueOfCurrentParam(currentParamValuesTail)
    }
  }

  /**
    * A helper function to filter a range of parameters based on the validation rules that can be
    * applied at this point. A rule can be applied only if all parameters it refers to either have been
    * processed already (i.e. if their values are in the accumulated combination), or are one of the
    * bounding parameters.
    */
  private def filterRange(param: Parameter[Any],
                          combination: Vector[Any],
                          paramsInCombination: Map[Parameter[Any], Int],
                          boundingParams: Option[Map[Parameter[Any], Any]],
                          validationRules: ValidationRules): Vector[Any] = {
    param.range.toVector.filter(value =>
      /* Verify that the current value complies with the validation rules */
      !validationRules.rulesPerParam.contains(param) ||   // No rules to check, so no errors found
        (validationRules.rulesPerParam(param) match {
          case Nil =>
            // No rules to check, so no errors found
            true
          case paramRules@_ =>
            paramRules.forall(rule => {
              val updatedCombination = combination :+ value
              // Check whether we either have processed all the parameters that this rule refers to, or have them
              // as bounding parameters
              if (!rule.params.forall(p => {
                paramsInCombination.contains(p) || (boundingParams match {
                  case Some(bParams) => bParams.contains(p)
                  case None => false})
              })) {
                true // Rule cannot be applied, so no errors found
              } else {
                // Get values of the corresponding parameters using the order that we keep in
                // the updatedParamsInCombination
                val referredValues: Vector[Any] = rule.params.toVector.map(param =>
                {
                  if (paramsInCombination.contains(param)) updatedCombination(paramsInCombination(param))
                  else {
                    val e = new IllegalArgumentException("The parameter the rule refers to neither has been " +
                      "processed, nor is one of the bounding parameters. This should have been caught earlier.")
                    boundingParams match {
                      case Some(bParams) => if (bParams.contains(param)) bParams(param) else throw e
                      case None => throw e
                    }}})
                rule.isValid(referredValues)
              }})}))
  }
}
