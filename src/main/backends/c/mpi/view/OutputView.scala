package backends.c.mpi.view

import backends.c.common.utils.output_view.OutputView.{init_body, post_check, pre_check}
import backends.c.common.utils.pattern_matching.IsDefinedAt
import backends.c.mpi.mpi_ir.BcastMPI
import ir.ast.{FunCall, IRNode, Lambda}
import ir.view.NoView

object OutputView {

  def generateOutputView(node: IRNode, cont: IRNode => IRNode): IRNode = {
    node match {


      case fc@FunCall(_:BcastMPI, arg) => {

        assert(fc.outputView != NoView)

        arg.outputView = fc.outputView

        assert(arg.outputView != NoView)

        cont( arg )

        fc
      }

    }
  }

  def composed_generateOutputView(in: IRNode) : IRNode = {

    val partial_binded_common = new PartialFunction[IRNode, IRNode] with IsDefinedAt[IRNode] {
      def apply(x: IRNode) = backends.c.common.view.OutputView.generateOutputView(x, composed_generateOutputView)
    }
    val partial_binded_mpi = new PartialFunction[IRNode, IRNode] with IsDefinedAt[IRNode] {
      def apply(x: IRNode) = backends.c.common.view.OutputView.generateOutputView(x, composed_generateOutputView)
    }
    val composed = partial_binded_common orElse partial_binded_mpi
    composed(in)

  }

  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_body(lambda)

    composed_generateOutputView( lambda.body )

    post_check(lambda)

  }

}
