package rewriting.rules

import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.debug.AssertType
import ir.ast.{Expr, FunCall, FunDecl, Gather, Get, Lambda, Lambda1, Let, ReorderWithStride, RewritingGuidePost, Split, UserFun, Value, Zip, asVector, λ}
import ir.ast.onnx.{AveragePool, ConvWithBias, ConvWithoutBias}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.ir.Float4
import opencl.ir.pattern.{MapLcl, MapSeq, ReduceSeq, toPrivate}
import patterns.nn.conv
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
        tuneParams = new ConvStencil3D.ConvStencil3DTuneParams(),
        rewriteParams = new conv.ConvStencil3D.ConvStencil3DRewriteParams(),
        fuseLambdas = false /* TODO: handle lambda fusion*/,
        shareKernels = false)(0), args: _*) // TODO: this throws away the final part of the expression
    })


    /** Pooling **/
    val averagePoolAsCPUFunc = Rule("AveragePool => <PoolCPU3D expression>", {
      case call @ FunCall(_: AveragePool, args @ _*) if args.length == 1 =>

        PoolCPU3D(call, call.args.head)
    })
  }

  object StencilRules {
    val vectorise4Marker = Rule("", {
      case FunCall(RewritingGuidePost("potentialAsVector"), arg) => asVector(4) $ arg
    })

    val vectorise4Chunks = Rule("", {
      case FunCall(RewritingGuidePost("chunkOfVectors"), FunCall(Split(s), arg)) =>
        Split(s /^ Cst(4)) $ arg
    })

    val vectorise4AssertType = Rule("", {
      case FunCall(RewritingGuidePost("windowType"),
      FunCall(AssertType(ArrayTypeWSWC(ArrayTypeWSWC(elemT, s1, c1), s2, c2), msg), arg)) =>
        AssertType(ArrayTypeWSWC(ArrayTypeWSWC(Float4, s1 /^ Cst(4), c1  /^ Cst(4)), s2, c2), msg) $ arg
    })

    val vectorise4Id = Rule("", {
      case FunCall(RewritingGuidePost("vectorisableId"), FunCall(uf: UserFun, ufArg))
        if uf.name == "id" =>
        UserFun("idF4", "x", "{ return x; }", Float4, Float4) $ ufArg
    })

    val vectorise4DotAndSumUp = Rule("", {
      case FunCall(RewritingGuidePost("vectorisableDotAndSumUp"), FunCall(uf: UserFun, ufArgs@ _*))
        if uf.name == "dotAndSumUp" =>

        FunCall(UserFun("dotAndSumUp_float4", Array("acc", "l", "r"), "{ return acc + dot(l, r); }",
          Seq(opencl.ir.Float, opencl.ir.Float4, opencl.ir.Float4), opencl.ir.Float), ufArgs: _*)
    })

    val vectorise4 = Rule("vectorise4", {
      case FunCall(callBody, FunCall(_: Zip, zipArgs@_*))
        if zipArgs.forall(zipArg => Rewrite.listAllPossibleRewrites(zipArg, vectorise4Marker).nonEmpty) =>

        val vectorisedZipArgs = zipArgs.map(zipArg => {
          Seq(
            vectorise4Marker,
            vectorise4Chunks,
            vectorise4AssertType).foldLeft[Expr](zipArg) {
            case (rewrittenExpr: Expr, rule: Rule) =>
              val rewrites = Rewrite.listAllPossibleRewrites(rewrittenExpr, rule)

              rewrites.foldLeft(rewrittenExpr) {
                case (rewrittenExpr: Expr, potentialRewrite: PotentialRewrite) =>
                  val replacement = potentialRewrite.rule.rewrite(potentialRewrite.expr)

                  Expr.replace(rewrittenExpr, potentialRewrite.expr, replacement)
              }
          }
        })

        val vectorisedCallBody = Seq(
          vectorise4Marker,
          vectorise4Id,
          vectorise4DotAndSumUp).foldLeft[Lambda](callBody) {
          case (rewrittenLambda: Lambda, rule: Rule) =>
            val rewrites = Rewrite.listAllPossibleRewrites(rewrittenLambda, rule)
            rewrites.foldLeft[Lambda](rewrittenLambda) {
              case (partRewrittenLambda: Lambda, potentialRewrite: PotentialRewrite) =>
                val replacement = potentialRewrite.rule.rewrite(potentialRewrite.expr)

                Lambda(partRewrittenLambda.params, Expr.replace(partRewrittenLambda.body, potentialRewrite.expr, replacement))
            }
        }

        FunCall(vectorisedCallBody, Zip(vectorisedZipArgs: _*))
    })

    def coalesce(stride: ArithExpr) = Rule("GuidePost(coalescing) => Gather(ReorderWithStride(s))", {
      case FunCall(RewritingGuidePost("coalescing"), arg) =>
        FunCall(Gather(ReorderWithStride(stride)), arg)
    })
  }
}
