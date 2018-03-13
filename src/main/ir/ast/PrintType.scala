package ir.ast

import ir.Type
import ir.interpreter.Interpreter._
import utils.paternoster.visualisation.TypeVisualiser

/**
  * A pattern for debugging Lift code.
  * Identity function that prints the Lift type of its input.
  * Generates no OpenCL code.
  */
case class PrintType(visual: Boolean = false, render: Boolean = false,expression:String = "") extends Pattern(arity = 1) {
  override def checkType(argType: Type,
                           setType: Boolean): Type = {

    if(visual){
      TypeVisualiser(argType,render,expression)
    }else{
      println(argType.toString)
    }

    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}
