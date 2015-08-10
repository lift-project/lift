package opencl.ir

import apart.arithmetic.ArithExpr
import ir.ast._

package object pattern {
  def Tile(size: ArithExpr): Lambda = Tile(size, size)

  def Tile(x: ArithExpr, y: ArithExpr) =
    Map(Map(Transpose()) o Split(y) o Transpose()) o Split(x)

  def Untile() = Join() o Map(Map(Join()) o TransposeW())

  def ReorderStride(s: ArithExpr) = Gather(reorderStride(s))
}
