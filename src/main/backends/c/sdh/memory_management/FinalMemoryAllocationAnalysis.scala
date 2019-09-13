package backends.c.sdh.memory_management


import backends.c.sdh.sdh_ir.{ToGPE, ToLCP}
import core.generator.GenericAST.CVarWithType
import ir.ast.{FunCall, IRNode, Lambda}
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace


/*
object FinalMemoryAllocationAnalysis {


  def analyze(node:IRNode) : (IRNode, Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]) = {
    node match {

      case fc@FunCall(_:ToLCP | _:ToGPE, arg) =>

        (node, Map.empty)


    }
  }

  def apply(lambda: Lambda): Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace) ] = {


    analyze(lambda.body)


  }

}*/
