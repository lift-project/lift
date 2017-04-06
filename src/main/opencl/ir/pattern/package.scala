package opencl.ir

import lift.arithmetic.ArithExpr
import ir.ast._

package object pattern {
  def Tile(size: ArithExpr): Lambda = Tile(size, size)

  def Tile(x: ArithExpr, y: ArithExpr) =
    Map(Map(Transpose()) o Split(y) o Transpose()) o Split(x)

  def Untile2D() = Join() o Map(Map(Join()) o TransposeW())

  def Untile3D() = Map(Map(Join())) o Map(Join()) o Join() o Map(Map(Map(TransposeW())))  o Map(TransposeW()) o Map(Map(TransposeW()))

  def ReorderStride(s: ArithExpr) = Gather(reorderStride(s))

}
