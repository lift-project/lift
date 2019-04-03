package opencl.ir

import ir.ScalarType
import ir.ast._
import opencl.generator.IllegalKernel
import opencl.ir.pattern._

object InferOpenCLAddressSpace {

  /**
    * Infer OpenCL address spaces for `lambda`.
    *
    * By default the parameters will be set to global space
    * and the return address space should be in global memory
    *
    * @param lambda The lambda to infer address spaces for
    */
  def apply(lambda: Lambda): Unit = {

    // Set the param address space to global memory, if it's not a scalar
    lambda.params.foreach(p => p.t match {
      case _: ScalarType => p.addressSpace = PrivateMemory
      case _ => p.addressSpace = GlobalMemory
    })

    setAddressSpace(lambda.body)
  }

  private def setAddressSpace(expr: Expr,
                              writeTo : OpenCLAddressSpace = UndefAddressSpace) : OpenCLAddressSpace = {

    val result = expr match {
      case Value(_, _) => PrivateMemory
      case _: ArrayConstructors => UndefAddressSpace
      case vp: VectorParam => vp.p.addressSpace
      case p: Param => p.addressSpace
      case f: FunCall => setAddressSpaceFunCall(f, writeTo)
    }

    expr.addressSpace = result
    result
  }

  private def setAddressSpaceFunCall(call: FunCall,
                                     writeTo: OpenCLAddressSpace) : OpenCLAddressSpace = {

    val addressSpaces = call.args.map(setAddressSpace(_, writeTo))

    call.f match {

      case RewritingGuidePost(_) =>
        setAddressSpaceDefault(addressSpaces)

      case Unzip() | Zip(_) | Transpose() | TransposeW() | asVector(_) |
           asScalar() | Split(_) | Join() | Scatter(_) | Gather(_) |
           Pad(_,_,_) | PadConstant(_, _, _) | Tuple(_) | Slide(_,_) | Head() | Tail() | debug.PrintType(_) |
           debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _) |
           UnsafeArrayAccess(_) | CheckedArrayAccess(_) | ArrayAccess(_) | Id() =>

        setAddressSpaceDefault(addressSpaces)

      case toPrivate(_) | toLocal(_) | toGlobal(_) =>
        setAddressSpaceChange(call, addressSpaces)

      case Filter() => addressSpaces.head
      case Get(i) => setAddressSpaceGet(i, addressSpaces.head)

      case iss: InsertionSortSeq => setAddressSpaceSort(iss, writeTo, addressSpaces)
      case fs: FilterSeq =>
        setAddressSpaceLambda(fs.f, PrivateMemory, addressSpaces)
        inferAddressSpace(writeTo, addressSpaces)

      case rw: ReduceWhileSeq => setAddressSpaceReduceWhile(rw, call, addressSpaces)
      case r: AbstractPartRed => setAddressSpaceReduce(r.f, call, addressSpaces)
      case s: AbstractSearch => setAddressSpaceSearch(s, writeTo, addressSpaces)

      case l: Lambda => setAddressSpaceLambda(l, writeTo, addressSpaces)
      case fp: FPattern => setAddressSpaceLambda(fp.f, writeTo, addressSpaces)

      case VectorizeUserFun(_, _) | UserFun(_, _, _, _, _) =>
        inferAddressSpace(writeTo, addressSpaces)

    }
  }

  private def setAddressSpaceDefault(addressSpaces: Seq[OpenCLAddressSpace]) =
    if (addressSpaces.length == 1) addressSpaces.head
    else AddressSpaceCollection(addressSpaces)

  private def setAddressSpaceGet(i: Int, addressSpace: OpenCLAddressSpace) =
    addressSpace match {
      case collection: AddressSpaceCollection => collection.spaces(i)
      case _ => addressSpace
    }
  
  private def setAddressSpaceSort(iss: InsertionSortSeq,
                             writeTo: OpenCLAddressSpace,
                             addrSpaces: Seq[OpenCLAddressSpace]) = {
    setAddressSpaceLambda(iss.f, PrivateMemory, addrSpaces)
    inferAddressSpace(writeTo, addrSpaces)
  }

  private def setAddressSpaceReduce(lambda: Lambda, call: FunCall,
    addressSpaces: Seq[OpenCLAddressSpace]) = {

    // First argument is initial value
    if (call.args.head.addressSpace == UndefAddressSpace)
      throw UnexpectedAddressSpaceException(
        s"No address space ${call.args.head.addressSpace} at $call")

    // The address space of the result of a reduction
    // is always the same as the initial element
    val writeTo = call.args.head.addressSpace

    setAddressSpaceLambda(lambda, writeTo, addressSpaces)
  }
  
  private def setAddressSpaceReduceWhile(rw: ReduceWhileSeq, call: FunCall,
                                         addressSpaces: Seq[OpenCLAddressSpace]) = {
    // default to just writing to private memory
    setAddressSpaceLambda(rw.p, PrivateMemory, addressSpaces)

    // carry on with the normal reduction address space setting
    setAddressSpaceReduce(rw.f, call, addressSpaces)
  }

  private def setAddressSpaceSearch(s: AbstractSearch, writeTo: OpenCLAddressSpace,
    addressSpaces: Seq[OpenCLAddressSpace]) = {

    // Set the comparison function input to be the
    // elements of the array we're searching
    s.f.params(0).addressSpace = addressSpaces(1)

    // Set address space for the comparison function
    setAddressSpace(s.f.body, PrivateMemory)

    inferAddressSpace(writeTo, addressSpaces)
  }

  private def setAddressSpaceLambda(l: Lambda, writeTo : OpenCLAddressSpace,
    addressSpaces : Seq[OpenCLAddressSpace]) = {

    l.params.zip(addressSpaces).foreach({ case (p, a) => p.addressSpace = a })
    setAddressSpace(l.body, writeTo)
  }

  private def setAddressSpaceChange(call:FunCall,
    addressSpaces : Seq[OpenCLAddressSpace]) = {

    if (!call.isConcrete(false))
      throw new IllegalKernel(s"Address space change requested without a write at $call")

    val (addressSpace, lambda) = call.f match {
      case toPrivate(f) => (PrivateMemory, f)
      case toLocal(f) => (LocalMemory, f)
      case toGlobal(f) => (GlobalMemory, f)
    }

    setAddressSpaceLambda(lambda, addressSpace, addressSpaces)
  }

  // TODO: Decide if this strategy is the one we want. See issue #61
  private def inferAddressSpace(writeTo : OpenCLAddressSpace,
    addressSpaces: Seq[OpenCLAddressSpace]) = {

    if (writeTo != UndefAddressSpace)
      writeTo
    else
      AddressSpaceCollection(addressSpaces).findCommonAddressSpace()

  }
}
