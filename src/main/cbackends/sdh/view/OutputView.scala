package cbackends.sdh.view


import cbackends.sdh.sdh_ir.{MapGPESync, TMKernel, ToGPE, ToLCP}
import ir.ast.{FunCall, IRNode, Lambda}
import cbackends.common.utils.output_view.OutputView.{pre_check,post_check,init_body}


object OutputView {

  def generateOutputView(node: Option[IRNode], cont: Option[IRNode] => Option[IRNode]): Option[IRNode] = {
    node match {

      case None => None

      case Some(fc@FunCall(_:ToGPE, arg) ) => {
        arg.outputView = fc.outputView;
        cont( Some(arg) )

        None
      }
      case Some(fc@FunCall(_:ToLCP, arg) ) => {
        arg.outputView = fc.outputView ;
        cont( Some(arg) )

        None
      }



      case Some(fc@FunCall(_:MapGPESync, arg) ) => {
        arg.outputView = fc.outputView ;
        cont( Some(arg) )

        None
      }
      case Some(fc@FunCall(tm:TMKernel, arg) ) => {

        tm.f.body.outputView = fc.outputView
        cont( Some(tm.f.body) )

        arg.outputView = fc.outputView
        cont( Some(arg) )

        None

      }


      case Some(_) => node
    }
  }

  def composed_generateOutputView(in: Option[IRNode]) : Option[IRNode] = {

    val partial_binded_common = cbackends.common.view.OutputView.generateOutputView(_:Option[IRNode], composed_generateOutputView)
    val partial_binded_sdh = cbackends.sdh.view.OutputView.generateOutputView(_:Option[IRNode], composed_generateOutputView)
    val composed = partial_binded_common andThen partial_binded_sdh andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)

  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_body(lambda)

    composed_generateOutputView( Some(lambda.body) )

    post_check(lambda)

  }

}

