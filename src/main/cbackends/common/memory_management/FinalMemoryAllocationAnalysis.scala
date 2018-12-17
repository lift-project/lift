package cbackends.common.memory_management

import core.generator.GenericAST.CVarWithType
import ir.ast.{AbstractMap, AbstractPartRed, FunCall, IRNode, Lambda, UserFun}
import lift.arithmetic.ArithExpr
import cbackends.common.utils.type_lowering.TypeLowering
import ir.Type

import scala.collection.mutable

object FinalMemoryAllocationAnalysis {

  var hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr) ]

  def analyze(node:IRNode): Unit = {
    node match {

      case fc@FunCall(_:UserFun, _*) =>
        hostMemoryDeclaredInSignature +=  fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ),  Type.getElementCount(fc.t) )

      case fc@FunCall(_:AbstractPartRed, _*) =>
        //correct type for user function, e.g., float => [float]_1
        hostMemoryDeclaredInSignature += fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) , Type.getElementCount(fc.t) )

      case fc@FunCall(_:AbstractMap, _*) =>
        //here fc.t already have the augmented size information after map, so no need to manually calculate
        hostMemoryDeclaredInSignature += fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) , Type.getElementCount(fc.t) )

      case _ =>

    }
  }

  def apply(lambda: Lambda): Map[String, (CVarWithType, ArithExpr) ] = {

    //reset hostMemoryDeclaredInSignature if run with multiple test cases
    hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr)]

    analyze(lambda)

    hostMemoryDeclaredInSignature.toMap

  }

}
