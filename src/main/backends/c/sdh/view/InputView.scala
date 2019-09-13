package backends.c.sdh.view

import backends.c.sdh.sdh_ir._
import backends.c.common.utils.input_view.InputView.{init_params, post_check, pre_check}
import backends.c.common.utils.pattern_matching.IsDefinedAt
import ir.ast.{FunCall, IRNode, Lambda}

object InputView {

  def generateInputView(node: IRNode , cont: IRNode => IRNode ) : IRNode = {
    node match {

      case fc@FunCall(_:ToGPE|_:ToLCP|_:MapGPESync, arg)  => {
        cont( arg )
        fc.view = arg.view
        fc
      }

      case fc@FunCall(tm:TMKernel, arg)  => {
        cont( arg)

        tm.f.params.head.view = arg.view
        cont( tm.f.body )

        fc.view = arg.view

        fc
      }

      case fc@FunCall(l:LCPSingle, arg)  => {
        cont( arg)

        l.f.params.head.view = arg.view
        cont( l.f.body )

        fc.view = arg.view

        fc
      }

    }
  }

  def composed_generateInputView(in: IRNode) : IRNode = {

    val partial_binded_common = new PartialFunction[IRNode, IRNode] with IsDefinedAt[IRNode]
      { def apply(x: IRNode) = backends.c.common.view.InputView.generateInputView(x, composed_generateInputView) }
    val partial_binded_sdh = new PartialFunction[IRNode,IRNode] with IsDefinedAt[IRNode]
      { def apply(x: IRNode) = backends.c.sdh.view.InputView.generateInputView(x, composed_generateInputView) }
    val composed = partial_binded_common orElse partial_binded_sdh
    composed(in)

  }


  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    composed_generateInputView( lambda.body )

    post_check(lambda)


  }


}
