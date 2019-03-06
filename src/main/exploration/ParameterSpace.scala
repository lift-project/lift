package exploration

import lift.arithmetic.{Cst, Var}
import _root_.utils.RangeValueGenerator

import scala.util.Random

/**
  * All information defining parameter space
  *
  * @param parameters unique parameters defining the space
  * @param validationRules the rules declaring what parameter value combinations are valid
  */
class ParameterSpace(val parameters: Vector[Var],
                     val validationRules: ValidationRules) {

  /** The order of parameter value generation is following: first process the parameters that are not referred to by
    * any validation rule, then the remaining parameters in their order of validation complexity as
    * defined in ValidationRules
    */
  def getNextRandomPoint(random: Random): Vector[Cst] = {

    val parametersWithoutValidation: Vector[Var] = parameters.filter(p =>
      !validationRules.validatedParamsSortedByComplexity.contains(p))

    // Generate values for parameters that need no validation
    val partialCombination: List[Int] = parametersWithoutValidation.map(param =>
      ParameterSpace.getRandomValueInRange(param, random)).toList

    // Generate values for parameters that need validation
    validationRules.validatedParamsSortedByComplexity.foldLeft((partialCombination, Map[Var, Int]()))(
      (state, currentParam) => {
        val combinationInProgress: List[Int] = state._1
        val paramsInCombination = state._2

        // Generate a random value for current parameter
        val valueAlreadyGenerated: Map[Int, Boolean] =
          RangeValueGenerator.generateAllValues(currentParam.range).map(valueInRange => valueInRange -> false).toMap

        // TODO: refactor with a more functional approach
        var validValueGenerated: Boolean = false
        var uniqueValuesGenerated: Int = 0
        var generatedValue: Int = null

        while (!validValueGenerated && uniqueValuesGenerated < valueAlreadyGenerated.size) {
          generatedValue = ParameterSpace.getRandomValueInRange(currentParam, random)
          if (!valueAlreadyGenerated(generatedValue)) {
            // The value has not been tried before. Let's check it against the validation rules
            valueAlreadyGenerated(generatedValue) = true
            uniqueValuesGenerated += 1

            if (validationRules.rulesPerParam(currentParam).forall(rule => {
              // Make sure all the values required by this rule have been generated. If this is not so, something has
              // gone wrong with parameter sorting.
              rule.params.foreach(ruleParam =>
                if (!paramsInCombination.contains(ruleParam))
                  throw new IllegalStateException(s"The parameter $ruleParam referenced by the rule $rule is not in the " +
                    s"partially generated combination which should have been ensured by sorting the parameters by " +
                    s"validation complexity. Check the parameter sorting algorithm in ValidationRules."))

              // Check that the rule is valid with the values generated previously or now
              rule.isValid(rule.params.map(ruleParam =>
                if (ruleParam.name == currentParam.name) // Generated now
                  generatedValue
                else // Generated previously
                  combinationInProgress(paramsInCombination(ruleParam))))
            })) {
              // The attempted value has passed the checks. On the next while condition check, we'll exit the loop
              validValueGenerated = true
            }
          }
        }


        if (!validValueGenerated) {
          throw new IllegalArgumentException(
            f"Could not generate a value for parameter $currentParam -- all unique values in range" +
              f" were generated in random order and failed the checks against validation rules")
        } else {
          // Return the updated state
          (combinationInProgress :+ generatedValue, // new value
            paramsInCombination + (currentParam -> combinationInProgress.length)) // new index
        }})
  }._1.map(intValue => Cst(intValue)).toVector
}

object ParameterSpace {
  def getRandomValueInRange(param: Var, random: Random): Int = {
    val randomIndexInRange = random.nextInt(param.range.max.evalInt)

    RangeValueGenerator.generateSingleValue(param.range, randomIndexInRange)
  }
}
