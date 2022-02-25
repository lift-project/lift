package rewriting.rules

import ir.ast.debug.AssertType
import ir.ast.onnx.{AveragePool, ConvWithBias, ConvWithoutBias}
import ir.ast.{Expr, FunCall, Gather, Lambda, ReorderWithStride, RewritingGuidePost, Split, UserFun, Zip, asVector}
import ir.{ArrayTypeWSWC, VectorType}
import lift.arithmetic.{ArithExpr, Var}
import opencl.ir.Float4
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
    def vectoriseMarker(vectorLen: ArithExpr) = Rule("", {
      case FunCall(RewritingGuidePost("potentialAsVector"), arg) => asVector(vectorLen) $ arg
    })

    def vectoriseChunks(vectorLen: ArithExpr) = Rule("", {
      case FunCall(RewritingGuidePost("chunkOfVectors"), FunCall(Split(s), arg)) =>
        Split(s /^ vectorLen) $ arg
    })

    def vectoriseAssertType(vectorLen: ArithExpr) = Rule("", {
//      case FunCall(RewritingGuidePost("arrayOfChunksType"),
//      FunCall(AssertType(ArrayTypeWSWC(ArrayTypeWSWC(elemT, s1, c1), s2, c2), msg), arg)) =>
//        AssertType(ArrayTypeWSWC(ArrayTypeWSWC(VectorType(opencl.ir.Float,
//          vectorLen), s1 /^ vectorLen, c1  /^ vectorLen), s2, c2), msg) $ arg
      case _ =>
        throw new NotImplementedError()
    })

    def vectoriseId(vectorLen: ArithExpr) = Rule("", {
      case FunCall(RewritingGuidePost("vectorisableId"), FunCall(uf: UserFun, ufArg))
        if uf.name == "id" =>
        UserFun("idF4", "x", "{ return x; }",
          VectorType(opencl.ir.Float, vectorLen), VectorType(opencl.ir.Float, vectorLen)) $ ufArg
    })

    def vectoriseDotAndSumUp(vectorLen: ArithExpr) = Rule("", {
      case FunCall(RewritingGuidePost("vectorisableDotAndSumUp"), FunCall(uf: UserFun, ufArgs@ _*))
        if uf.name == "dotAndSumUp" =>

        FunCall(UserFun("dotAndSumUp_float_vectorLen", Array("acc", "l", "r"), "{ return acc + dot(l, r); }",
          Seq(opencl.ir.Float, VectorType(opencl.ir.Float, vectorLen),
            VectorType(opencl.ir.Float, vectorLen)), opencl.ir.Float), ufArgs: _*)
    })

    def vectorise(vectorLen: ArithExpr) = Rule("vectorise", {
      case FunCall(callBody, FunCall(_: Zip, zipArgs@_*))
        if zipArgs.forall(zipArg => Rewrite.listAllPossibleRewrites(zipArg, vectoriseMarker(vectorLen)).nonEmpty) =>

        val vectorisedZipArgs = zipArgs.map(zipArg => {
          Seq(
            vectoriseMarker(vectorLen),
            vectoriseChunks(vectorLen),
            vectoriseAssertType(vectorLen)).foldLeft[Expr](zipArg) {
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
          vectoriseMarker(vectorLen),
          vectoriseId(vectorLen),
          vectoriseDotAndSumUp(vectorLen),
          vectoriseAssertType(vectorLen)).foldLeft[Lambda](callBody) {
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
