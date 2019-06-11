package cbackends.sdh.view


import cbackends.sdh.sdh_ir._
import ir.ast.{FunCall, IRNode, Lambda}
import cbackends.common.utils.output_view.OutputView.{init_body, post_check, pre_check}
import cbackends.common.utils.pattern_matching.IsDefinedAt


object OutputView {

  def generateOutputView(node: IRNode, cont: IRNode => IRNode): IRNode = {
    node match {


      case fc@FunCall(_:ToGPE|_:ToLCP|_:MapGPESync, arg)  => {
        arg.outputView = fc.outputView;
        cont( arg )

        fc
      }
        /*
      case fc@FunCall(_:ToLCP, arg)  => {
        arg.outputView = fc.outputView ;
        cont( arg )

        fc
      }



      case fc@FunCall(_:MapGPESync, arg)  => {
        arg.outputView = fc.outputView ;
        cont( arg )

        fc
      }*/
      case fc@FunCall(tm:TMKernel, arg)  => {

        tm.f.body.outputView = fc.outputView
        cont( tm.f.body )

        arg.outputView = fc.outputView
        cont( arg )

        fc

      }

      case fc@FunCall(l:LCPSingle, arg)  => {

        l.f.body.outputView = fc.outputView
        cont( l.f.body )

        arg.outputView = fc.outputView
        cont( arg )

        fc

      }


    }
  }

  def composed_generateOutputView(in: IRNode) : IRNode = {

    val partial_bineded_common = new PartialFunction[IRNode,IRNode] with IsDefinedAt[IRNode] {
      def apply(x: IRNode) = cbackends.common.view.OutputView.generateOutputView(x:IRNode, composed_generateOutputView)
    }
    val partial_bineded_sdh = new PartialFunction[IRNode,IRNode] with IsDefinedAt[IRNode] {
      def apply(x: IRNode) = cbackends.sdh.view.OutputView.generateOutputView(x:IRNode, composed_generateOutputView)
    }
    val composed = partial_bineded_common orElse partial_bineded_sdh
    composed(in)


  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_body(lambda)

    composed_generateOutputView( lambda.body )

    post_check(lambda)

  }

}

