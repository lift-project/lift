package cbackends.onnx.lowering

//import ir.ast.nnet.Conv
import cbackends.onnx.lift_nn_ir.Conv
import cbackends.onnx.lift_nn_ir.host_ir.{Conv3D, Pool3D}
import ir.ast.onnx.{AveragePool, ConvWithoutBias}
import ir.ast.{Expr, FunCall, FunDecl, IRNode, Lambda, Param}
import opencl.ir.id

object LoweringONNXIR2LiftHostIR {


  def transform(expr: Expr) : Expr = {

    expr match {
      case fc @ FunCall(c:ConvWithoutBias, args@_*) =>
        //assert(c.kernel_shape.length == 3)
        //FunCall(Conv3D(), args:_* )
        //FunCall(Conv3D(), args(0) )
        //transform_conv_without_bias(fc)
        assert(args.length == 2)
        Conv3D(fc, transform(fc.args(0)), transform(fc.args(1) ))
      case fc @ FunCall(p:AveragePool, arg) =>
        Pool3D(fc, transform(arg))
      case x =>
        x
    }

  }



  def apply(lambda: Lambda): Lambda = {

    Lambda(lambda.params, transform(lambda.body) )

  }

}
