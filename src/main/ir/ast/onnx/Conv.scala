package ir.ast.onnx

import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, Cst}
import sun.reflect.generics.reflectiveObjects.NotImplementedException


abstract class AbstractConv(override val arity: Int,
                            autoPad: String,
                            dilations: List[ArithExpr],
                            group: ArithExpr,
                            kernelShape: List[ArithExpr],
                            pads: List[ArithExpr],
                            strides: List[ArithExpr]) extends Pattern(arity) {

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }
}





case class ConvWithoutBias(auto_pad: String,
                           dilations: List[ArithExpr],
                           group: ArithExpr,
                           kernel_shape: List[ArithExpr],
                           pads: List[ArithExpr],
                           strides: List[ArithExpr])
  extends AbstractConv(2, auto_pad, dilations, group, kernel_shape, pads, strides) {

  override def checkType(argType: Type, setType: Boolean): Type = argType



}


case class ConvWithBias (autoPad: String,
                                dilations: List[ArithExpr],
                                group: ArithExpr,
                                kernelShape: List[ArithExpr],
                                pads: List[ArithExpr],
                                strides: List[ArithExpr])
  extends AbstractConv(3, autoPad, dilations, group, kernelShape, pads, strides) {

  override def checkType(argType: Type, setType: Boolean): Type = argType

}

