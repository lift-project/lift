package patterns

import ir.ast.Expr
import lift.arithmetic.ArithExpr

package object nn {
  trait LayerParams[+T <: ArithExpr] {
    val paramVector: Vector[T]
  }
  abstract class LayerConfig[+T <: ArithExpr] extends LayerParams[T]
  abstract class LayerTuneParams[+T <: ArithExpr] extends LayerParams[T]

  abstract class LayerExpression[
  T1 <: LayerConfig[ArithExpr],
  T2 <: LayerTuneParams[ArithExpr]](layerConfig: T1,
                                    tuneParams: T2)
}
