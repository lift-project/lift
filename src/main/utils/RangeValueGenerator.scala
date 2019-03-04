package utils

import lift.arithmetic.{Range, RangeAdd, RangeMul}

object RangeValueGenerator {
  def generateSingleValue(range: Range, i: Int): Int = {
    range match {
      case rAdd: RangeAdd =>
        assert(i < rAdd.max.evalInt)
        rAdd.start.evalInt + rAdd.step.evalInt * i

      case rMul: RangeMul =>
        assert(i >= rMul.min.evalInt)
        assert(i < rMul.max.evalInt)
        rMul.start.evalInt * Math.pow(rMul.mul.evalInt, i).toInt

      case r@_ =>
        throw new NotImplementedError(s"Range $r is unsupported by RangeGenerator")
    }
  }

  def generateAllValues(range: Range): Vector[Int] = {
    range match {
      case rAdd: RangeAdd =>
        val start = rAdd.start.evalInt
        val step = rAdd.step.evalInt
        val stop = rAdd.stop.evalInt
        (start to stop by step).toVector
      case rMul: RangeMul =>
        val min = rMul.min.evalInt
        val max = rMul.max.evalInt
        val start = rMul.start.evalInt
        val mul = rMul.mul.evalInt
        (min to max).scan(start)((previousValue, _) => previousValue * mul).toVector

      case r@_ =>
        throw new NotImplementedError(s"Range $r is unsupported by RangeGenerator")
    }
  }
}
