package ir

import ir.ast._

object GenerateIR{

  // 0 -> f, 1 -> Map(f), 2 -> Map(Map(f)), ...
  def wrapInMaps(f: => Lambda)(count: Int): Lambda = {
    if(count < 1) f
    else Map(wrapInMaps(f)(count-1))
  }

  // creates Map(...(Map(f)...) o ... o Map(f) o f
  def applyInEveryDimUntilDim(f: => Lambda)(dim: Int): Lambda = {
    if(dim <= 1) f
    else applyInEveryDimUntilDim(Map(f))(dim-1) o f
    //else Map(applyInEveryDimUntilDim(f)(dim-1)) o f <- fused-maps version
  }

  // creates f o Map(f) o ... o Map(...(Map(f)...)
  def applyInEveryDimUntilDimReverse(f: => Lambda)(dim: Int): Lambda = {
    if(dim <= 1) f
    else f o applyInEveryDimUntilDimReverse(Map(f))(dim-1)
    //else f o Map(applyInEveryDimUntilDimReverse(f)(dim-1)) <- fused-maps version
  }
}
