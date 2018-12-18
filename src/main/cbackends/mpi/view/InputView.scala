package cbackends.mpi.view

import cbackends.mpi.mpi_ir.BcastMPI
import ir.ast.{FunCall, IRNode, Lambda}
import cbackends.common.utils.input_view.InputView.{pre_check,post_check,init_params}

object InputView {

  def generateInputView(node: Option[IRNode] , cont: Option[IRNode] => Option[IRNode] ) : Option[IRNode] = {
    node match {

      case None => None

      case Some(fc@FunCall(_:BcastMPI, arg) ) => {
        cont( Some(arg) )
        fc.view = arg.view
        None
      }

      case Some(_) => node

    }
  }

  def composed_generateInputView(in: Option[IRNode]) : Option[IRNode] = {

    val partial_binded_common = cbackends.common.view.InputView.generateInputView(_:Option[IRNode], composed_generateInputView)
    val partial_binded_mpi = cbackends.mpi.view.InputView.generateInputView(_:Option[IRNode], composed_generateInputView)
    val composed = partial_binded_common andThen partial_binded_mpi andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)

  }


  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    composed_generateInputView(Some(lambda.body) )

    post_check(lambda)


  }

}
