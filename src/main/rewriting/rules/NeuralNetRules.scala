package rewriting.rules

import ir.ast.{Expr, FunCall, FunDecl, Get, Lambda, Lambda1, Let, UserFun, Value, Zip, asVector, λ}
import ir.ast.onnx.{AveragePool, ConvWithBias, ConvWithoutBias}
import opencl.ir.Float4
import opencl.ir.pattern.{MapLcl, MapSeq, ReduceSeq, toPrivate}
import patterns.nn.conv.{ConvCPU3D, ConvStencil3D}
import patterns.nn.pool.PoolCPU3D
import rewriting.{PotentialRewrite, Rewrite}

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
    val vectorise4Funs = Rule("" , {
      case FunCall(uf: UserFun, ufArgs@_*) if uf.name == "dotAndSumUp" =>

        FunCall(UserFun("dotAndSumUp_float4", Array("acc", "l", "r"), "{ return acc + dot(l, r); }",
          Seq(opencl.ir.Float, opencl.ir.Float4, opencl.ir.Float4), opencl.ir.Float), ufArgs: _*)


      case FunCall(MapSeq(Lambda1(_, FunCall(toPrivate(Lambda(_, FunCall(uf: UserFun, _), _)), _))),
      mapSeqArg @ FunCall(_: Get, _)) if uf.name == "id" =>

        val idF4 = UserFun("idF4", "x", "{ return x; }", Float4, Float4)

        MapSeq(toPrivate(idF4)) $ mapSeqArg
    })

    val vectorise4 = Rule("", {
      case FunCall(mapLcl0Call @ MapLcl(0, mapLcl0Body @ Lambda1(_, FunCall(_: Let, letArgs))), FunCall(_: Zip, zipArgs @ _*))
      if Rewrite.listAllPossibleRewrites(mapLcl0Call, vectorise4Funs).nonEmpty =>
        val vectorisedZipArg0 = ir.ast.Map(asVector(4)) $ zipArgs(0)
        val vectorisedZipArg1 = ir.ast.Map(ir.ast.Map(asVector(4))) $ zipArgs(1)

        val vectorisedZip = Zip(vectorisedZipArg0, vectorisedZipArg1)

//        val vectorisedMapLcl0Call = mapLcl0Call $ vectorisedZip

        val userFunRewrites = Rewrite.listAllPossibleRewrites(mapLcl0Call, vectorise4Funs)

        val rewrittenMapLcl0Body = userFunRewrites.foldLeft[Lambda](mapLcl0Body) {
          case (rewrittenLambda: Lambda, potentialRewrite: PotentialRewrite) =>
            val replacement = potentialRewrite.rule.rewrite(potentialRewrite.expr)

            Lambda(rewrittenLambda.params, Expr.replace(rewrittenLambda.body, potentialRewrite.expr, replacement))
        }

        MapLcl(0)(rewrittenMapLcl0Body) $ vectorisedZip
    })

  }
}
