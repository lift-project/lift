package ir

import ir.ast._

object GenerateIR{

  // 0 -> f, 1 -> Map(f), 2 -> Map(Map(f)), ...
  def wrapInMaps(f: => Lambda, count: Int): Lambda = {
    if(count < 1) f
    else Map(wrapInMaps(f, count-1))
  }

  // creates Map(...(Map(f)...) o ... o Map(f) o f
  def applyInEveryDimUntilDim(f: => Lambda, dim: Int): Lambda = {
    if(dim <= 1) f
    else applyInEveryDimUntilDim(Map(f), dim-1) o f
    //else Map(applyInEveryDimUntilDim(f)(dim-1)) o f <- fused-maps version
  }

  // creates f o Map(f) o ... o Map(...(Map(f)...)
  def applyInEveryDimUntilDimReverse(f: => Lambda, dim: Int): Lambda = {
    if(dim <= 1) f
    else f o applyInEveryDimUntilDimReverse(Map(f), dim-1)
    //else f o Map(applyInEveryDimUntilDimReverse(f)(dim-1)) <- fused-maps version
  }

  // [a][A][b][B][c][C]... => [a][b][c]...[A][B][C]...
  def interleaveDimensions(count: Int, i: Int): Lambda = {
    val howManyMaps = -2 * (count - 1 - i) - 1
    if(i < 2) throw new IllegalArgumentException("Not enough dimensions to interleave")
    if(count == 2) GenerateIR.wrapInMaps(Transpose(), howManyMaps)
    else {
      GenerateIR.applyInEveryDimUntilDim(GenerateIR.wrapInMaps(Transpose(), howManyMaps), count - 1) o
        interleaveDimensions(count - 1, i)
    }
  }

  def interleaveDimensionsReverse(dim: Int): Lambda = {
    if(dim < 2) throw new IllegalArgumentException("Not enough dimensions to interleave")
    if(dim == 2) Map(Transpose())
    else interleaveDimensionsReverse(dim -1) o
      GenerateIR.applyInEveryDimUntilDim(GenerateIR.wrapInMaps(Transpose(), dim -1), dim-1)
  }
}
