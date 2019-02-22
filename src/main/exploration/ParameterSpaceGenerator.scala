package exploration

object ParameterSpaceGenerator {
  //  def apply(params: Seq[Parameter[Any]]): _ = {
  def main(args: Array[String]): Unit = {
    val p1 = Parameter("width", 1 to 4)
    val p2 = Parameter("height", 2 to 5)
    val p3 = Parameter("memory", Seq(true, false))
    val r1 = ValidationRule[Int, Int](p1, p2, (p1val: Int, p2val: Int) => p1val <= p2val)
    val r2 = ValidationRule[Int, Int, Boolean](p1, p2, p3,
      (p1val: Int, p2val: Int, p3val: Boolean) =>
        if (p3val) p1val == p2val else p1val != p2val)

    val rules = ValidationRules(Seq(r1, r2))
    //    println(rules)

    val space = apply(Seq(p1, p2, p3), rules)
    println(space)
    space.foreach{(combination) =>
      combination.foreach{(value) =>
        print(value + " ")
      }
      println()
    }
    println()
  }

  def apply(params: Seq[Parameter[Any]], validationRules: ValidationRules): Stream[Seq[Any]] = {
    combineParameters(Map[Parameter[Any], Int](), Stream.empty, params.head, params.tail, validationRules)
  }

  def combineParameters(paramsInCombination: Map[Parameter[Any], Int], combination: Seq[Any],
                        currentParam: Parameter[Any], remainingParams: Seq[Parameter[Any]],
                        validationRules: ValidationRules): Stream[Seq[Any]] = {
    val updatedParamsInCombination = paramsInCombination + (currentParam -> paramsInCombination.size)

    remainingParams match {
      case Nil =>
        finishCombinations(combination,
          filterRange(currentParam, combination, updatedParamsInCombination, validationRules))

      case nextParam :: remainingParamsTail =>
        filterRange(currentParam, combination, updatedParamsInCombination, validationRules).
          foldLeft(Stream[Seq[Any]]())(
            /* Build combinations using lazily verified values */
            (combinationCollection, value) => {

              /* Generate combinations using the current value and the yet unprocessed parameters */
              val partialCombinationCollection = combineParameters(
                updatedParamsInCombination,
                combination :+ value,
                nextParam,
                remainingParamsTail,
                validationRules)

              /* Merge combinations produced using previous values of the current parameter and the ones we just
                 generated */
              combinationCollection match {
                case Stream.Empty => partialCombinationCollection
                case _ => combinationCollection ++ partialCombinationCollection
              }
            })
    }
  }

  def finishCombinations(combination: Seq[Any], remainingParamValues: Stream[Any]): Stream[Seq[Any]] = {
    remainingParamValues match {
      case Stream.Empty => Stream.empty
      case value #:: rest =>
        val nextCombination = finishCombinations(combination, rest)
        nextCombination match {
          case Stream.Empty => Stream(combination :+ value)
          case _ => Stream(combination :+ value) ++ nextCombination
        }
    }
  }

  def filterRange(param: Parameter[Any], combination: Seq[Any], paramsInCombination: Map[Parameter[Any], Int],
                  validationRules: ValidationRules): Stream[Any] = {
    param.range.toStream.filter(value =>
      /* Verify that the current value complies with the validation rules */
      validationRules.rulesPerParam(param) match {
        case Nil =>
          // No rules to check, so no errors found
          true
        case paramRules@_ =>
          paramRules.forall(rule => {
            val updatedCombination = combination :+ value
            // Check whether we have processed all the parameters that this rule refers to
            if (!rule.params.forall(p => paramsInCombination.contains(p))) {
              true // Rule cannot be applied, so no errors found
            } else {
              // Get values of the corresponding parameters using the order that we keep in
              // the updatedParamsInCombination
              val referredValues: Seq[Any] = rule.params.map(param =>
                updatedCombination(paramsInCombination(param)))
              rule.isValid(referredValues)
            }})})
  }
}
