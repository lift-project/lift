package ir.ast.onnx

import ir.Type
import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import sun.reflect.generics.reflectiveObjects.NotImplementedException

abstract class Pool(
                     override val arity: Int
                   ) extends Pattern(arity){

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}


case class AveragePool (
                       auto_pad : String =  "NOTSET",
                       count_include_pad : Int = 0,
                       kernel_shape : List[ArithExpr],
                       pads : List[ArithExpr],
                       strides : List[ArithExpr]

                       ) extends Pool(1) {


  override def checkType(argType: Type, setType: Boolean): Type = argType

}

