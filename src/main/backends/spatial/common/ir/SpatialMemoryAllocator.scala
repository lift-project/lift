package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern._
import _root_.ir.ast.{AbstractSearch, ArrayAccess, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Concat, Expr, Filter, FunCall, Gather, Get, Head, Id, Iterate, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector, debug}
import _root_.ir.{NumberOfArgumentsException, Type, UnallocatedMemory}
import lift.arithmetic.ArithExpr

object SpatialMemoryAllocator {
  /**
    * Allocate memory for both the body and the parameters of the outermost lambda expression
    *
    * @param f the lambda expression
    * @return the SpatialMemory used as output by f
    */
  def apply(f: Lambda): SpatialMemory = {
    f.params.foreach((p) =>
      p.mem = SpatialMemory.allocMemory(p.t, p.addressSpace)
    )

    alloc(f.body, f.body.t)
  }

  /**
    * Allocate SpatialMemory objects for a given expression
    *
    * @param expr      The expression for which memory should be allocated
    * @param writeT    The type of the output memory which the expression is writing to
    * @return          The SpatialMemory used by expr
    */
  def alloc(expr: Expr, writeT: Type): SpatialMemory = {

    val result = expr match {
      case ArrayFromExpr(e)       => throw new NotImplementedError()
      case _: ArrayConstructors   => throw new NotImplementedError()

      case v: Value               => allocValue(v)
      case vp: VectorParam        => throw new NotImplementedError()
      case p: Param               => allocParam(p)
      case call: FunCall          => allocFunCall(call, writeT)
      case _                      => throw new NotImplementedError()
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
      SpatialMemory.allocRegMemory(v.t)
    }
  }

  private def allocParam(p: Param): SpatialMemory = {
    if (p.mem == UnallocatedMemory)
      throw new IllegalArgumentException(s"Param $p has UnallocatedMemory")

    SpatialMemory.asSpatialMemory(p.mem)
  }

  private def allocFunCall(call: FunCall,
                           writeT: Type): SpatialMemory = {
    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens
      case _: UserFun               => allocUserFun(writeT, call)
      case  _: VectorizeUserFun     => throw new NotImplementedError()

      case sf: SpForeach            => allocSpForeach(sf, writeT, inMem)

      case asf: AbstractSpFold      => allocSpFold(asf, writeT, inMem)

      case s: AbstractSearch        => throw new NotImplementedError()

      case it: Iterate              => throw new NotImplementedError()

      case cc: Concat               => throw new NotImplementedError()

      case l: Lambda                => allocLambda(l, writeT, inMem)
      // For address space casters, set the new output memory type
      // since we will buffer the lambda output in the new address space
      // and then transfer the buffered data to the old address space separately
      case toDRAM(f)                => allocLambda(f, call.t, inMem)
      case toSRAM(f)                => allocLambda(f, call.t, inMem)
      case toReg(f)                 => allocLambda(f, call.t, inMem)

      case Zip(_) | Tuple(_)        => allocZipTuple(inMem)
      case Get(n)                   => allocGet(n, inMem)
      case f: Filter                => throw new NotImplementedError()
      case ua: UnsafeArrayAccess    => throw new NotImplementedError()
      case ca: CheckedArrayAccess   => throw new NotImplementedError()

      case debug.PrintView(_, f)    => allocLambda(f, writeT, inMem)

      case RewritingGuidePost(_)    => inMem

      case Map(_) |
           Split(_) | Join() | asVector(_) | asScalar() |
           Transpose() | Unzip() | TransposeW() | Slide(_, _) | Pad(_, _, _) | PadConstant(_, _, _) |
           Head() | Tail() | Gather(_) | Scatter(_) | ArrayAccess(_) |
           debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _) |
           Id()                     => inMem

      case _                        => throw new NotImplementedError()
    }
  }

  private def getInMFromArgs(call: FunCall): SpatialMemory = {
    call.args.length match {
      case 0 => throw new IllegalArgumentException(s"Function call without arguments $call")
      case 1 => alloc(call.args.head, call.args.head.t)
      case _ => SpatialMemoryCollection(call.args.map(arg => alloc(arg, arg.t)))
    }
  }

  private def allocUserFun(writeT: Type,
                           call: FunCall): SpatialMemory = {
    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    SpatialMemory.allocMemory(writeT, call.addressSpace)
  }

  private def allocLambda(l: Lambda,
                          writeT: Type,
                          inMem: SpatialMemory): SpatialMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, writeT)
  }

  private def allocSpForeach(sf: SpForeach,
                             writeT: Type,
                             inMem: SpatialMemory): SpatialMemory = {
    sf.f.params(0).mem = inMem

    alloc(sf.f.body, writeT)
  }

  private def allocSpFold(asf: AbstractSpFold,
                          writeT: Type,
                          inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        val initM = coll.subMemories(0)

        val inputArrType = asf.fMap.params(0).t

        asf.fMap.params(0).mem = coll.subMemories(1)
        // fMap body memory will be referred to by fReduce, but will not be allocated --
        // Spatial will pass the results of fMap to fReduce by value
        val mapBodyMem = alloc(asf.fMap.body, inputArrType)

        asf.fReduce.params(0).mem = initM
        asf.fReduce.params(1).mem = mapBodyMem

        val reduceBodyM = alloc(asf.fReduce.body, writeT)

        // replace `bodyM` by `initM` in `asf.fReduce.body`
        Expr.visit(asf.fReduce.body, e => if (e.mem == reduceBodyM) e.mem = initM, _ => {})

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

  private def setMemInParams(params: Array[Param], inMem: SpatialMemory): Unit = {
    params.length match {
      case 1 => params.head.mem = inMem
      case _ =>
        val coll = inMem.asInstanceOf[SpatialMemoryCollection]
        (params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
  }
}
