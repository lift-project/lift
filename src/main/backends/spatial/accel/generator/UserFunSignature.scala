package backends.spatial.accel.generator

import _root_.ir.AddressSpace
import _root_.ir.ast.UserFun
import _root_.ir.ast.FunCall
import backends.spatial.common.Printer
import backends.spatial.common.SpatialAST.{ExprBasedFunction, SpParamDecl, SpatialCode}
import core.generator.GenericAST.MutableExprBlock

/**
 * This class and its companion object provide representation of UserFuns in Spatial Scala.
 * Special care is taken to overload functions correctly when required.
 */
case class UserFunSignature(uf: UserFun,
                            argSpatialTypes: List[String],
                            argAss: List[AddressSpace],
                            retAs: AddressSpace,
                            returnSpatialType: String,
                            var uniqueName: String) {
  /**
   * Defines equality using Spatial types ignoring Lift types since Spatial types also express memory spaces
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: UserFunSignature =>
      uf.name.equals(that.uf.name) &&
        argSpatialTypes == that.argSpatialTypes &&
        returnSpatialType.equals(that.returnSpatialType)
    case _ => false
  }

  override def hashCode: Int = uf.name.toUpperCase.hashCode

  /**
   * Decides whether this function is indistinguishable from the argument function,
   * which would mean that the two functions cannot overload each other
   */
  def cannotBeOverloadedWith(that: UserFunSignature): Boolean =
    uf.name.equals(that.uf.name) &&
      argSpatialTypes.length == that.argSpatialTypes.length

  def createFunctionDefinition(): ExprBasedFunction = {
    val block = MutableExprBlock()
    if (uf.tupleTypes.length == 1)
      throw new NotImplementedError()
    else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
      throw new NotImplementedError()
    })

    block += SpatialCode(uf.body)
    ExprBasedFunction(
      name = uniqueName,
      ret = uf.outT,
      addressSpace = retAs,
      params = (uf.inTs, uf.paramNames, argAss).
        zipped.map((t, n, as) => SpParamDecl(n, t, as)).toList,
      body = block)
  }
}

object UserFunSignature {
  def apply(uf: UserFun, call: FunCall): UserFunSignature =
    new UserFunSignature(uf,
      call.args.map(arg => Printer.toString(arg.t, arg.addressSpace)).toList, call.args.map(_.addressSpace).toList,
      call.addressSpace, Printer.toString(uf.outT, call.addressSpace),
      uf.name)

  /**
   * Finds the argument function in the collection and returns its unique name
   */
  def getUniqueName(uf: UserFun, call: FunCall, signatureCollection: Set[UserFunSignature]): String = {
    val nonUniqueSignature = apply(uf, call)

    signatureCollection.find(_.equals(nonUniqueSignature)) match {
      case Some(uniqueSignature) => uniqueSignature.uniqueName
      case None =>
        throw new IllegalArgumentException(f"Function $uf in the call $call does not exist in the signature collection")
    }
  }
}
