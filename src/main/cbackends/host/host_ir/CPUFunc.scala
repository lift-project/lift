package cbackends.host.host_ir

import ir.ast.{FPattern, IDGenerator, IRNode, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir.{Type, TypeChecker}

case class CPUFunc(override val f: Lambda, val funcName: String = "execute_" + IDGenerator.get_id()) extends Pattern(arity = 1) with FPattern {

  override def copy(f: Lambda): Pattern = CPUFunc(f, funcName)


  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    f.params(0).t = argType
    TypeChecker.check(f.body, setType)
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => a.map(f.eval(valueMap, _))
    }
  }
}

case class CPUFunCall(funcName : String) extends Pattern(arity = 1) {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}
