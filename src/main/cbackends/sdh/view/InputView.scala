package cbackends.sdh.view

import cbackends.sdh.sdh_ir.{MapGPESync, TMKernel, ToGPE, ToLCP}
import cbackends.common.view.InputView.{pre_check, post_check, init_params}
import ir.ast.{FunCall, IRNode, Lambda}

object InputView {

  def generateInputView(node: Option[IRNode] , cont: Option[IRNode] => Option[IRNode] ) : Option[IRNode] = {
    node match {

      case None => None

      case Some(fc@FunCall(_:ToGPE, arg) ) => {
        cont( Some(arg) )
        fc.view = arg.view
        None
      }
      case Some(fc@FunCall(_:ToLCP, arg) ) => {
        cont( Some(arg) )
        fc.view = arg.view
        None
      }

      case Some(fc@FunCall(_:MapGPESync, arg) ) => {
        cont( Some(arg) )
        fc.view = arg.view
        None
      }
      case Some(fc@FunCall(tm:TMKernel, arg) ) => {
        cont( Some(arg) )

        tm.f.params.head.view = arg.view
        cont( Some(tm.f.body) )

        fc.view = arg.view

        None
      }

      case Some(_) => node
    }
  }

  def composed_generateInputView(in: Option[IRNode]) : Option[IRNode] = {

    val partial_binded_common = cbackends.common.view.InputView.generateInputView(_:Option[IRNode], composed_generateInputView)
    val partial_binded_sdh = cbackends.sdh.view.InputView.generateInputView(_:Option[IRNode], composed_generateInputView)
    val composed = partial_binded_common andThen partial_binded_sdh andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)

  }


  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    composed_generateInputView(Some(lambda.body) )

    post_check(lambda)


  }


}
