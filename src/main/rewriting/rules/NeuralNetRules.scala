package rewriting.rules

import ir.ast.{FunCall, Get, Lambda, UserFun, Zip, asVector, λ}
import ir.ast.onnx.{AveragePool, ConvWithBias, ConvWithoutBias}
import opencl.ir.pattern.ReduceSeq
import patterns.nn.conv.{ConvCPU3D, ConvStencil3D}
import patterns.nn.pool.PoolCPU3D

/**
  * Neural Network-specific rewriting rules
  */
object NeuralNetRules {

  /*** Per-frontend lowering rules ***/
  object ONNXLoweringRules {
    /** Convolution **/
    // This rule only works for convolutions with the stride of 1
    val convWithoutBiasAsCPUFunc = Rule("ConvWithoutBias => <ConvCPU3D expression>", {
      case call @ FunCall(_: ConvWithoutBias, args@ _*) if args.length == 2 =>

        ConvCPU3D(call, call.args.head, call.args(1))
    })

    val convWithBiasAsStencil = Rule("ConvWithBias => <ConvStencil3D expression>", {
      case call @ FunCall(onnxNode: ConvWithBias, args@ _*) if args.length == 3 =>
      FunCall(ConvStencil3D(
//        layerConfig = ConvStencil3D.ConvStencil3DLayerConfig(onnxNode, args.head),
        layerConfig = new ConvStencil3D.ConvStencil3DLayerConfig(),
        tuneParams = new ConvStencil3D.ConvStencil3DTuneParams())(0), args: _*) // TODO: this throws away the final part of the expression
    })


    /** Pooling **/
    val averagePoolAsCPUFunc = Rule("AveragePool => <PoolCPU3D expression>", {
      case call @ FunCall(_: AveragePool, args @ _*) if args.length == 1 =>

        PoolCPU3D(call, call.args.head)
    })
  }

  object StencilRules {
    val vectorise4 = Rule("ReduceSeq(dotAndSumUp) $ Zip(a0, a1) => " +
      "ReduceSeq(dotAndSumUpF4) $ Zip(asVector(a0), asVector(a1))", {
      case FunCall(ReduceSeq(Lambda(_, FunCall(uf: UserFun, _*), _)), reduceInit, FunCall(_: Zip, zipArgs @ _*))
        if uf.name == "dotAndSumUp" =>

        val vectorisedZipArgs = zipArgs.map(arg => asVector(4) $ arg)

        val vectorisedUserFun = UserFun("dotAndSumUp_float4", Array("acc", "l", "r"), "{ return acc + dot(l, r); }",
          Seq(opencl.ir.Float, opencl.ir.Float4, opencl.ir.Float4), opencl.ir.Float)

        ReduceSeq(λ((acc, y) => vectorisedUserFun(acc, Get(y, 0), Get(y, 1))), reduceInit) $ Zip(vectorisedZipArgs: _*)
    })
  }
}
