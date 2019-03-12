package utils

import lift.arithmetic.{Range, RangeAdd, RangeMul}

object RangeValueGenerator {

  def rangeSize(range: Range): Int = {
    range match {
      case r: RangeAdd =>
        r.numVals.evalInt
      case RangeMul(start, stop, mul) =>
        // TODO: implement more robustly as RangeMul.numVals
        // This assumes that both stop and start are powers of mul
        log(stop, mul) - log(start, mul)
      case r => throw new IllegalArgumentException(s"Range $r is not supported by getRandomValueInRange")
    }
  }

  def generateSingleValue(range: Range, i: Int): Int = {
    assert(i < rangeSize(range))
    range match {
      case rAdd: RangeAdd =>    rAdd.start.evalInt + rAdd.step.evalInt * i
      case rMul: RangeMul =>    rMul.start.evalInt * Math.pow(rMul.mul.evalInt, i).toInt
      case r@_ =>               throw new NotImplementedError(s"Range $r is unsupported by RangeGenerator")
    }
  }

  def generateAllValues(range: Range): Vector[Int] = {
    range match {
      case rAdd: RangeAdd =>
        val start = rAdd.start.evalInt
        val step = rAdd.step.evalInt
        val stop = rAdd.stop.evalInt
        (start until stop by step).toVector
      case rMul: RangeMul =>
        val start = rMul.start.evalInt
        val stop = rMul.stop.evalInt
        val mul = rMul.mul.evalInt
        (start until (Math.log(stop) / Math.log(mul)).toInt).scan(start)((previousValue, _) => previousValue * mul).toVector

      case r@_ =>
        throw new NotImplementedError(s"Range $r is unsupported by RangeGenerator")
    }
  }
}
