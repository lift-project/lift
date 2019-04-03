package patterns

import ir.ast.Expr
import lift.arithmetic.ArithExpr

package object nn {
  trait LayerParams[+T <: ArithExpr] {
    val paramVector: Vector[T]
  }
  abstract class LayerConfig[+T <: ArithExpr] extends LayerParams[T]
  abstract class LayerTuneParams[+T <: ArithExpr] extends LayerParams[T]

  trait LayerExpressionFactory {
    // TODO: generalize types
    def apply(layerConfig: ConvStencil3DLayerConfig,
              tuneParams: ConvStencil3DTuneParams): Seq[Lambda]
  }

  type Array2D[T] = Array[Array[T]]
  type Array3D[T] = Array[Array[Array[T]]]
  type Array4D[T] = Array[Array[Array[Array[T]]]]
  type Array5D[T] = Array[Array[Array[Array[Array[T]]]]]
  type Array6D[T] = Array[Array[Array[Array[Array[Array[T]]]]]]
  type Array7D[T] = Array[Array[Array[Array[Array[Array[Array[T]]]]]]]
}
