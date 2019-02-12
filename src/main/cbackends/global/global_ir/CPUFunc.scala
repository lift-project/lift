package cbackends.global.global_ir


import ir.Type
import ir.ast.{AbstractMap, FPattern, IRNode, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.PosVar

case class CPUFunc(override val f: Lambda) extends Pattern(arity = 1) with FPattern {

  override def copy(f: Lambda): Pattern = CPUFunc(f)


  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => a.map(f.eval(valueMap, _))
    }
  }
}
