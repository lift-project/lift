package patterns.nn.utils

import lift.arithmetic.{ArithExpr, floor}

object Utils {
  /**
    * Computes the number of output elements in sliding (tiling, convolution, pooling, etc)
    * @param areaSize the size of the data we are sliding (e.g. spatial input size)
    * @param sliderSize the size of the sliding window (e.g. kernel or tile slide)
    * @param sliderStride the stride of sliding (for overlapping sliding, the stride is less than sliderSize)
    * @return
    */
  def slidingOutputSize(areaSize: ArithExpr, sliderSize: ArithExpr, sliderStride: ArithExpr): ArithExpr =
//    (areaSize - (sliderSize - sliderStride)) /^ sliderStride
    (areaSize - (sliderSize - sliderStride)) / sliderStride
}
