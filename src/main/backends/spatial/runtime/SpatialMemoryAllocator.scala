package backends.spatial.runtime

import arithmetic.TypeVar
import backends.spatial.accel.ir.pattern.{MapPar, MapSeq, toDRAM, toReg, toSRAM}
import backends.spatial.ir.{RegMemory, SpatialAddressSpace, SpatialMemory, SpatialMemoryCollection, UndefAddressSpace}
import backends.spatial.ir.SpatialAddressSpace.asSpatialAddressSpace
import ir.Type.size_t
import ir.{ArrayType, Capacity, NumberOfArgumentsException, Type, UnallocatedMemory}
import ir.ast.{AbstractMap, AbstractPartRed, AbstractSearch, ArrayAccess, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Concat, Expr, Filter, FunCall, Gather, Get, Head, Id, Iterate, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector, debug}
import lift.arithmetic.{?, ArithExpr}

object SpatialMemoryAllocator {
  /** (baseSize, innerSize) => totalSize */
  type Allocator = (ArithExpr, ArithExpr) => ArithExpr
  case class Allocators(dramSize: Allocator = (_, x) => x,
                        sramSize: Allocator = (_, x) => x,
                        regSize:  Allocator = (_, x) => x)

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
    * @param expr      The expression for which memory should be allocated
   *  @param memSizes  Functions computing the number of bytes to allocate for each memory
   *                   given the sizes of the elements and of the base elements when
   *                   the memory stores an array
    * @return          The SpatialMemory used by expr
    */
  def alloc(expr: Expr,
            memSizes: Allocators = Allocators()): SpatialMemory = {

    val result = expr match {
      case ArrayFromExpr(e) => throw new NotImplementedError()
      case _: ArrayConstructors => throw new NotImplementedError()

      case v: Value => allocValue(v)
      case vp: VectorParam => throw new NotImplementedError()
      case p: Param => allocParam(p)
      case call: FunCall =>
        allocFunCall(call, memSizes, UndefAddressSpace)
      case _ => throw new NotImplementedError()
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
                           memSizes: Allocators,
                           addressSpace: SpatialAddressSpace): SpatialMemory = {
    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, memSizes)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens
      case _: UserFun =>
        allocUserFun(call.t, memSizes, call)
      case  _: VectorizeUserFun => throw new NotImplementedError()

      case MapPar(_) | MapSeq(_)  => allocMapParSeq(call.f.asInstanceOf[AbstractMap], call.t, memSizes, inMem)

      case r: AbstractPartRed => allocReduce(r, memSizes, inMem)

      case s: AbstractSearch => throw new NotImplementedError()

      case it: Iterate => throw new NotImplementedError()

      case cc: Concat => throw new NotImplementedError()

      case l: Lambda => allocLambda(l, memSizes, inMem)
      case toDRAM(f) => allocLambda(f, memSizes, inMem)
      case toSRAM(f) => allocLambda(f, memSizes, inMem)
      case toReg(f)  => allocLambda(f, memSizes, inMem)

      case Zip(_) | Tuple(_) => allocZipTuple(inMem)
      case Get(n) => allocGet(n, inMem)
      case f: Filter => throw new NotImplementedError()
      case ua: UnsafeArrayAccess => throw new NotImplementedError()
      case ca: CheckedArrayAccess => throw new NotImplementedError()

      case debug.PrintView(_, f) => allocLambda(f, memSizes, inMem)

      case RewritingGuidePost(_) => inMem

      case Map(_) |
           Split(_) | Join() | asVector(_) | asScalar() |
           Transpose() | Unzip() | TransposeW() | Slide(_, _) | Pad(_, _, _) | PadConstant(_, _, _) |
           Head() | Tail() | Gather(_) | Scatter(_) | ArrayAccess(_) |
           debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _) |
           Id() =>
        inMem

      case _ => throw new NotImplementedError()
    }
  }

  private def getInMFromArgs(call: FunCall,
                             memSizes: Allocators): SpatialMemory = {
    call.args.length match {
      case 0 =>
        throw new IllegalArgumentException(s"Function call without arguments $call")
      case 1 =>
        alloc(call.args.head, memSizes)
      case _ =>
        SpatialMemoryCollection(call.args.map(alloc(_, memSizes)))
    }
  }

  private def allocUserFun(outT: Type,
                           memSizes: Allocators,
                           call: FunCall): SpatialMemory = {
    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    val maxSizeInBytes = Type.getAllocatedSize(outT)
    val baseSize = Type.getAllocatedSize(Type.getBaseType(outT))
    // size in bytes necessary to hold the result of f in the different
    // memory spaces
    val maxDRAMSize = memSizes.dramSize(baseSize, maxSizeInBytes)
    val maxSRAMSize = memSizes.sramSize(baseSize, maxSizeInBytes)
    val maxRegSize = memSizes.regSize(baseSize, maxSizeInBytes)

    SpatialMemory.allocMemory(maxDRAMSize, maxSRAMSize, maxRegSize, call.addressSpace)
  }

  private def allocLambda(l: Lambda,
                          memSizes: Allocators,
                          inMem: SpatialMemory): SpatialMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, memSizes)
  }

  private def allocMapParSeq(am: AbstractMap,
                             outT: Type,
                             memSizes: Allocators,
                             inMem: SpatialMemory): SpatialMemory = {

    val regMemSizeMultiplier: ArithExpr =
      if (am.f.body.addressSpace.containsAddressSpace(RegMemory) ||
        inMem.addressSpace.containsAddressSpace(RegMemory))
        am.iterationCount
      else
        1

    alloc(am.f.body, Allocators(
      dramSize = sizeOfArray(memSizes.dramSize, outT),
      sramSize = sizeOfArray(memSizes.sramSize, outT),
      regSize = (bs, inner) => memSizes.regSize(bs, regMemSizeMultiplier * inner)
    ))
  }

  private def allocReduce(r: AbstractPartRed,
                          memSizes: Allocators,
                          inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        val initM = coll.subMemories(0)
        r.f.params(0).mem = initM
        r.f.params(1).mem = coll.subMemories(1)
        val bodyM = alloc(r.f.body, memSizes)

        // replace `bodyM` by `initM` in `r.f.body`
        Expr.visit(r.f.body, e => if (e.mem == bodyM) e.mem = initM, _ => {})

        initM // return initM as the memory of the reduction pattern
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocZipTuple(inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        if (coll.subMemories.length < 2) throw new NumberOfArgumentsException
        coll
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocGet(n: Int, inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        assert(n < coll.subMemories.length)
        coll.subMemories(n)
      case _ => inMem
    }
  }

  private def setMemInParams(params: Array[Param], mem: SpatialMemory): Unit = {
    params.length match {
      case 1 => params.head.mem = mem
      case _ =>
        val coll = mem.asInstanceOf[SpatialMemoryCollection]
        (params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
  }

  /**
   * Helper function for computing the size to allocate for an array given its
   * type and the size of its elements
   *
   * @param getSizeFromInner a scala function allocating the whole array given the inner size
   * @param ty the type of the array
   * @return a scala function that allocates this array. Might be `?`
   */
  private def sizeOfArray(getSizeFromInner: Allocator, ty: Type): Allocator = {
    def align(n: ArithExpr): ArithExpr = ((n + size_t.size - 1) / size_t.size) * size_t.size

    ty match {
      case at: ArrayType =>
        at match {
          case c: Capacity =>
            val map = TypeVar.getTypeVars(c.capacity).map(tv => (tv, tv.range.max)).toMap
            val capacity = ArithExpr.substitute(c.capacity, map.toMap)
            def alloc(baseSize: ArithExpr, innerSize: ArithExpr): ArithExpr = {
              if (baseSize.eval < size_t.size.eval)
                getSizeFromInner(baseSize, at.headerSize * size_t.size + align(capacity * innerSize))
              else
                getSizeFromInner(baseSize, at.headerSize * baseSize + capacity * innerSize)
            }
            alloc
          case _ =>
            // Unknown capacity. We can't know how much memory to allocate…
            // See the comments at the top of this file for further information.
            (_, _) => ?
        }
      case _ => throw new IllegalArgumentException("sizeOfArray expects an array type")
    }
  }
}
