package cbackends.mpi.view

import cbackends.common.utils.output_view.OutputView.{init_body, post_check, pre_check}
import cbackends.mpi.mpi_ir.BcastMPI
import ir.ast.{FunCall, IRNode, Lambda}
import ir.view.NoView

object OutputView {

  def generateOutputView(node: Option[IRNode], cont: Option[IRNode] => Option[IRNode]): Option[IRNode] = {
    node match {

      case None => None

      case Some(fc@FunCall(_:BcastMPI, arg)) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont( Some(arg) )

        None
      }

      case Some(_) => node
    }
  }

  def composed_generateOutputView(in: Option[IRNode]) : Option[IRNode] = {

    val partial_binded_common = cbackends.common.view.OutputView.generateOutputView(_:Option[IRNode], composed_generateOutputView)
    val partial_binded_mpi = cbackends.mpi.view.OutputView.generateOutputView(_:Option[IRNode], composed_generateOutputView)
    val composed = partial_binded_common andThen partial_binded_mpi andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)

  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_body(lambda)

    composed_generateOutputView( Some(lambda.body) )

    post_check(lambda)

  }

}
