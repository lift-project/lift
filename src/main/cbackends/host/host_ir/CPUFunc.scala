package cbackends.host.host_ir

import ir.ast.{FPattern, IDGenerator, IRNode, Lambda, Param, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir.{TupleType, Type, TypeChecker}
import opencl.ir.TypedOpenCLMemory


/*
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

case class CPUFunCall(funcName : String, params: Array[Param]) extends Pattern(arity = 1) {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = {
    assert(false, "CPUFunCall* never do type check")
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}
*/

//accept arbitrary parameters.
case class CPUFunc(override val f: Lambda, val funcName: String = "execute_" + IDGenerator.get_id(), override val cpu_timer: Boolean = false)
  extends Pattern(arity = f.params.length) with FPattern with CPUMeasurable {

  override def copy(f: Lambda): Pattern = CPUFunc(f, funcName)


  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt:TupleType =>
        (f.params zip tt.elemsT).foreach( pair => pair._1.t = pair._2  )
      case _ =>
        f.params(0).t = argType
    }
    TypeChecker.check(f.body, setType)
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => a.map(f.eval(valueMap, _))
    }
  }
}

case class CPUFunContainer(cpuFun: CPUFunc, intermediateGlobalMem: Seq[TypedOpenCLMemory]) extends Pattern(arity = 2) {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = {
    assert(false, "CPUFunContainer* never do type check")
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}
