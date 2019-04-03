package patterns

import ir.ast.{FunDecl, Lambda}
import lift.arithmetic.Var
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}

package object nn {
  trait LayerParams {
    val paramVector: Vector[Var]
  }
  abstract class LayerConfig extends LayerParams
  abstract class LayerTuneParams extends LayerParams

  abstract class LayerExpression(layerConfig: LayerConfig,
                                 tuneParams: LayerTuneParams)

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
