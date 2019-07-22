package ir.ast.debug

import ir.Type
import ir.ast.Pattern
import ir.interpreter.Interpreter._
import utils.paternoster.visualisation.TypeVisualiser


/**
  * Small datatype to differetiate between textual or visual output
  */
sealed trait TypeOutput

case class VisualOutput(render: Boolean = false, expr: String = "") extends TypeOutput

case object TextOutput extends TypeOutput

/**
  * A pattern for debugging Lift code.
  * Identity function that prints the Lift type of its input.
  * Generates no OpenCL code.
  */
case class PrintType(outputType: TypeOutput = TextOutput) extends Pattern(arity = 1) {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    outputType match {
      case v: VisualOutput => {
        TypeVisualiser(argType, v.render, v.expr)
      }
      case TextOutput => println(argType.toString)
    }

    argType
  }


  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}