package backends.spatial.runtime

import backends.spatial.ir.{RegMemory, SpatialAddressSpace, SpatialMemory}
import ir.{Type, UnallocatedMemory}
import ir.ast.{AbstractMap, ArrayConstructors, ArrayFromExpr, Expr, FunCall, Get, Join, Lambda, Map, Param, Split, Tuple, UserFun, Value, VectorParam, VectorizeUserFun, Zip}
import lift.arithmetic.ArithExpr
import opencl.ir.UndefAddressSpace
import opencl.ir.pattern.MapSeq

object SpatialMemoryAllocator {
  /** (baseSize, innerSize) => totalSize */
  type Allocator = (ArithExpr, ArithExpr) => ArithExpr

  /**
    * Allocate memory for both the body and the parameters of a lambda
    * expression
    *
    * @param f the lambda expression
    * @return the SpatialMemory used as output by f
    */
  def apply(f: Lambda): SpatialMemory = {
    f.params.foreach((p) =>
      p.mem = SpatialMemory.allocMemory(Type.getAllocatedSize(p.t), p.addressSpace)
    )

    alloc(f.body)
  }

  /**
    * Allocate SpatialMemory objects for a given expression
    *
    * @param expr   The expression for which memory should be allocated
    * @param numGlb Function computing the number of bytes to allocate in global
    *               memory given the sizes of the elements and of the base
    *               elements in case this is an array.
    * @param numLcl Idem in local memory
    * @param numPvt Idem in private memory
    * @return The SpatialMemory used by expr
    */
  def alloc(expr: Expr,
            numGlb: Allocator = (_, x) => x,
            numLcl: Allocator = (_, x) => x,
            numPvt: Allocator = (_, x) => x): SpatialMemory = {

    val result = expr match {
      case ArrayFromExpr(e) => throw new NotImplementedError()
      case _: ArrayConstructors => throw new NotImplementedError()

      case v: Value => allocValue(v)
      case vp: VectorParam => throw new NotImplementedError()
      case p: Param => allocParam(p)
      case call: FunCall =>
        allocFunCall(call, numGlb, numLcl, numPvt, UndefAddressSpace)
    }
    // set the output
    expr.mem = result

    // finally, return the output
    result
  }

  private def allocValue(v: Value): SpatialMemory = {
    if (v.mem != UnallocatedMemory) {
      val spatialMem = SpatialMemory.asSpatialMemory(v.mem)
      assert(spatialMem.addressSpace == RegMemory)
      spatialMem
    } else {
      SpatialMemory.allocRegMemory(Type.getAllocatedSize(v.t))
    }
  }

  private def allocParam(p: Param): SpatialMemory = {
    if (p.mem == UnallocatedMemory)
      throw new IllegalArgumentException(s"Param $p has UnallocatedMemory")

    SpatialMemory.asSpatialMemory(p.mem)
  }

  private def allocFunCall(call: FunCall,
                           numGlb: Allocator,
                           numLcl: Allocator,
                           numPvt: Allocator,
                           addressSpace: SpatialAddressSpace): SpatialMemory = {
    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, numGlb, numLcl, numPvt)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens
      case _: UserFun | _: VectorizeUserFun =>
        allocUserFun(call.t, numGlb, numLcl, numPvt, call)

      case Map(_) => allocMapGlb(call.f.asInstanceOf[AbstractMap],
        call.t, numGlb, numLcl, numPvt, inMem)

      case MapSeq(_) =>
        allocMapSeq(call.f.asInstanceOf[AbstractMap],
          call.t, numGlb, numLcl, numPvt, inMem)

      case l: Lambda => allocLambda(l, numGlb, numLcl, numPvt, inMem)

      case Zip(_) | Tuple(_) => allocZipTuple(inMem)
      case Get(n) => allocGet(n, inMem)

      case Split(_) | Join() =>
        inMem

      case _ => throw new NotImplementedError()
    }
  }

  private def allocMapSeq(sp: MapSeqSlide,
                               outT: Type,
                               numGlb: Allocator,
                               numLcl: Allocator,
                               numPvt: Allocator,
                               inMem: OpenCLMemory): OpenCLMemory = {

    sp.f.params(0).mem = OpenCLMemory(sp.windowVar, Type.getAllocatedSize(sp.f.params(0).t) * sp.size , PrivateMemory)

    val privateMultiplier: ArithExpr =
      if (sp.f.body.addressSpace.containsAddressSpace(PrivateMemory) ||
        inMem.addressSpace.containsAddressSpace(PrivateMemory))
        sp.iterationCount
      else
        1

    alloc(sp.f.body,
      sizeOfArray(numGlb, outT),
      sizeOfArray(numLcl, outT),
      (bs, inner) => numPvt(bs, privateMultiplier * inner))
  }
}
