package cbackends.onnx.lowering

//import ir.ast.nnet.Conv
import cbackends.onnx.lift_nn_ir.Conv
import ir.ast.onnx.ConvWithoutBias
import ir.ast.{Expr, FunCall, IRNode, Lambda}
import opencl.ir.id

object LoweringONNXIR2LiftIR {


  def tranform(node: IRNode) : IRNode = {

    node match {
      case Lambda(params,body) =>
        Lambda(params, tranform(body).asInstanceOf[Expr] )
      case fc @ FunCall(c:ConvWithoutBias, _*) =>
        assert(c.kernel_shape.length == 3)
        Conv.Seq()
      case x =>
        x
    }

  }


  def apply(lambda: Lambda): Lambda = {

    tranform(lambda).asInstanceOf[Lambda]

  }

}
