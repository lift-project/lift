package backends.spatial.common.ir

import backends.spatial.accel.generator.IllegalAccelBlock
import backends.spatial.accel.ir.pattern.{AbstractSpFold, toDRAM, toLiteral, toReg, toSRAM}
import ir.ScalarType
import ir.ast.{AbstractPartRed, AbstractSearch, ArrayAccess, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Concat, Expr, FPattern, Filter, FunCall, Gather, Get, Head, Id, Join, Lambda, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector, debug}

object InferSpatialAddressSpace {

  /**
   * Infer Spatial address spaces for `lambda`.
   *
   * By default the parameters will be set to DRAM space
   * and the return address space should be in DRAM memory
   *
   * @param lambda The lambda to infer address spaces for
   */
  def apply(lambda: Lambda): Unit = {

    // Set the param address space to DRAM memory, if it's not a scalar
    lambda.params.foreach(p => p.t match {
      case _: ScalarType => p.addressSpace = RegMemory
      case _ => p.addressSpace = DRAMMemory
    })

    setAddressSpace(lambda.body)
  }

  private def setAddressSpace(expr: Expr,
                              writeTo: SpatialAddressSpace = UndefAddressSpace): SpatialAddressSpace = {

    val result = expr match {
      case Value(_, _) => if (writeTo == LiteralMemory) LiteralMemory else RegMemory
      case ArrayFromExpr(e)=> throw new NotImplementedError()
      case _: ArrayConstructors => throw new NotImplementedError()
      case vp: VectorParam => throw new NotImplementedError()
      case p: Param => p.addressSpace
      case f: FunCall => setAddressSpaceFunCall(f, writeTo)
    }

    expr.addressSpace = result
    result
  }

  private def setAddressSpaceFunCall(call: FunCall,
                                     writeTo: SpatialAddressSpace): SpatialAddressSpace = {

    val argAddressSpaces = call.args.map(setAddressSpace(_, writeTo))

    call.f match {
      case RewritingGuidePost(_)    => setAddressSpaceDefault(argAddressSpaces)

      case Concat(_)                => throw new NotImplementedError()


      case Unzip() | Zip(_) | Transpose() | TransposeW() | asVector(_) |
           asScalar() | Split(_) | Join() | Scatter(_) | Gather(_) |
           Pad(_,_,_) | PadConstant(_, _, _) | Tuple(_) | Slide(_,_) | Head() | Tail() | debug.PrintType(_) |
           debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _) |
           UnsafeArrayAccess(_) | CheckedArrayAccess(_) | ArrayAccess(_) | Id()
                                    => setAddressSpaceDefault(argAddressSpaces)

      case toDRAM(_) | toSRAM(_) |
           toReg(_) | toLiteral()   => setAddressSpaceChange(call, argAddressSpaces)

      case Filter()                 => throw new NotImplementedError()
      case Get(i)                   => setAddressSpaceGet(i, argAddressSpaces.head)

      case asf: AbstractSpFold      => setAddressSpaceFold(asf, call, writeTo, argAddressSpaces)
//      case r: AbstractPartRed       => setAddressSpaceReduce(r.f, call, argAddressSpaces)
      case s: AbstractSearch        => throw new NotImplementedError()

      case l: Lambda                => setAddressSpaceLambda(l, writeTo, argAddressSpaces)
      case fp: FPattern             => setAddressSpaceLambda(fp.f, writeTo, argAddressSpaces)

      case VectorizeUserFun(_, _)   => throw new NotImplementedError()
      case UserFun(_, _, _, _, _)   => inferAddressSpace(writeTo, argAddressSpaces)
    }
  }

  private def setAddressSpaceDefault(argAddressSpaces: Seq[SpatialAddressSpace]): SpatialAddressSpace =
    if (argAddressSpaces.length == 1) argAddressSpaces.head
    else AddressSpaceCollection(argAddressSpaces)

  private def setAddressSpaceGet(i: Int, argAddressSpace: SpatialAddressSpace): SpatialAddressSpace =
    argAddressSpace match {
      case collection: AddressSpaceCollection => collection.spaces(i)
      case _ => argAddressSpace
    }

  private def setAddressSpaceFold(asf: AbstractSpFold, call: FunCall,
                                  writeTo: SpatialAddressSpace,
                                  argAddressSpaces: Seq[SpatialAddressSpace]): SpatialAddressSpace = {
    val accumulatorAddressSpace = call.args.head.addressSpace

    val fMapAddressSpace = setAddressSpaceLambda(asf.fMap, accumulatorAddressSpace, Seq(argAddressSpaces(1)))

    // First argument is the initial value
    if (call.args.head.addressSpace == UndefAddressSpace)
      throw UnexpectedAddressSpaceException(s"No address space ${call.args.head.addressSpace} at $call")

    // The address space of the result of a reduction is always the same as the initial element
    val foldWriteTo = accumulatorAddressSpace

    setAddressSpaceLambda(asf.fReduce, foldWriteTo, Seq(argAddressSpaces.head, fMapAddressSpace))
  }

  private def setAddressSpaceLambda(l: Lambda, writeTo: SpatialAddressSpace,
                                    argAddressSpaces: Seq[SpatialAddressSpace]): SpatialAddressSpace = {
    l.params.zip(argAddressSpaces).foreach({ case (p, a) => p.addressSpace = a })
    setAddressSpace(l.body, writeTo)
  }

  private def setAddressSpaceChange(call: FunCall,
                                    argAddressSpaces: Seq[SpatialAddressSpace]): SpatialAddressSpace = {

    if (!call.f.isInstanceOf[toLiteral] && !call.isConcrete(false))
      throw new IllegalAccelBlock(s"Address space change requested without a write at $call")

    if (argAddressSpaces.length > 1)
      throw new IllegalAccelBlock(s"Expected only one argument to the address space caster")

    call.f match {
      case toLiteral() => LiteralMemory
      case _ =>
        val (addressSpace, lambda) = call.f match {
          case toReg(f) => (RegMemory, f)
          case toSRAM(f) => (SRAMMemory, f)
          case toDRAM(f) => (DRAMMemory, f)
        }

        setAddressSpaceLambda(lambda, argAddressSpaces.head, argAddressSpaces)

        addressSpace
    }
  }

  private def inferAddressSpace(writeTo: SpatialAddressSpace,
                                argAddressSpaces: Seq[SpatialAddressSpace]) = {

    if (writeTo != UndefAddressSpace)
      writeTo
    else AddressSpaceCollection(argAddressSpaces).findCommonAddressSpace()
  }
}
