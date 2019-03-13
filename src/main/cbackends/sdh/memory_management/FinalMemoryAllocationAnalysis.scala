package cbackends.sdh.memory_management


import core.generator.GenericAST.CVarWithType
import ir.ast.{IRNode, Lambda}
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace

import scala.collection.mutable

/*
object FinalMemoryAllocationAnalysis {

  var hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr, OpenCLAddressSpace) ]

  def analyze(node:IRNode): Unit = {
    node match {


    }
  }

  def apply(lambda: Lambda): Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace) ] = {

    //reset hostMemoryDeclaredInSignature if run with multiple test cases
    hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]

    analyze(lambda.body)

    hostMemoryDeclaredInSignature.toMap

  }

}*/
