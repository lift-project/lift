package cbackends.common.memory_management

import core.generator.GenericAST.CVarWithType
import ir.ast.{AbstractMap, AbstractPartRed, FunCall, Get, IRNode, Join, Lambda, Pad, Slide, Split, Transpose, TransposeW, UserFun, Zip}
import lift.arithmetic.ArithExpr
import cbackends.common.utils.type_lowering.TypeLowering
import cbackends.host.host_ir.CPUFunCall
import ir.Type

import scala.collection.mutable

object FinalMemoryAllocationAnalysis {

  var hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr) ]

  def analyze(node:IRNode): Unit = {
    node match {

      case fc@FunCall(_:UserFun|_:CPUFunCall, args@_*) =>
        args.foreach(analyze(_))
        hostMemoryDeclaredInSignature +=  fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ),  Type.getElementCount(fc.t) )

      case fc@FunCall(r:AbstractPartRed, args@_*) =>
        args.foreach(analyze(_))
        analyze(r.f.body)
        //correct type for user function, e.g., float => [float]_1
        hostMemoryDeclaredInSignature += fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) , Type.getElementCount(fc.t) )

      case fc@FunCall(m:ir.ast.Map, args@_*) =>
        // no memory correction for Map, which is a lazy struct
        // no need to traverse into Map, also because it is a lazy struct, no memory alloc can happen, thus no need for correction
        args.foreach(analyze(_))
        //analyze(m.f.body)

      case fc@FunCall(m:AbstractMap, args@_*) =>
        args.foreach(analyze(_))
        analyze(m.f.body)
        //here fc.t already have the augmented size information after map, so no need to manually calculate
        hostMemoryDeclaredInSignature += fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) , Type.getElementCount(fc.t) )

      case fc@FunCall(l:Lambda, args@_*) =>
        args.foreach(analyze(_))
        analyze(l.body)

      case fc@FunCall(_:Join|_:Slide|_:Zip|_:Get|_:Split|_:Join|_:Transpose|_:TransposeW|_:Pad, args@_*) =>
        args.foreach(analyze(_))

      case fc@FunCall(_, args@_*) =>
        assert(false)

      case _ =>

    }
  }

  def apply(lambda: Lambda): Map[String, (CVarWithType, ArithExpr) ] = {

    //reset hostMemoryDeclaredInSignature if run with multiple test cases
    hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr)]

    analyze(lambda.body)

    hostMemoryDeclaredInSignature.toMap

  }

}
