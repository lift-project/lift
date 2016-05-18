package opencl.ir

import ir.ScalarType
import ir.ast.{AbstractPartRed, Expr, FPattern, Filter, FunCall, Gather, Get, Group, Head, IRNode, Join, Lambda, Pad, Param, Scatter, Split, Tail, Transpose, TransposeW, Tuple, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector}
import opencl.ir.pattern.{toGlobal, toLocal, toPrivate}

/**
  * Created by Toomas Remmelg on 17/05/16.
  */
object InferOpenCLAddressSpace {

  private def addAddressSpace(e: Expr, as : OpenCLAddressSpace) = {
    e.addressSpaces = as
    e.addressSpaces
  }

  // by default the parameters of the lambda will be set to global space
  // and the return address space should be in global memory
  def apply(lambda: Lambda) = {
    // clear up all the address spaces first
    IRNode.visit(lambda, {
      case e: Expr => e.addressSpaces = UndefAddressSpace
      case _ =>
    })

    // set the param address space to global memory, if it's not a scalar
    lambda.params.foreach(p => p.t match {
      case _: ScalarType => p.addressSpaces = PrivateMemory
      case _ => p.addressSpaces = GlobalMemory
    })

    setAddressSpace(lambda.body)

    // TODO: Should this be moved? Could make unittesting easier
    // TODO: IllegalKernel exception seems more informative to me
    // TODO: Should at least include a proper explanation, why expected global
    if  (lambda.body.addressSpaces != GlobalMemory )
      throw UnexpectedAddressSpaceException(
        lambda.body.addressSpaces.toString,
        GlobalMemory.toString
      )
  }



  private def setAddressSpaceLambda(
    l: Lambda,
    writeTo : OpenCLAddressSpace,
    argsAddrSpace : Seq[OpenCLAddressSpace])
  = {

    l.params.zip(argsAddrSpace).foreach({case (p,a) =>
      p.addressSpaces = a
    })

    setAddressSpace(l.body, writeTo)
  }

  private def inferFunCallWriteTo(writeTo : OpenCLAddressSpace, addSpaces: Set[OpenCLAddressSpace]) = {
    if (writeTo == UndefAddressSpace) {
      if (addSpaces.contains(GlobalMemory))
        GlobalMemory
      else if (addSpaces.contains(LocalMemory))
        LocalMemory
      else if (addSpaces.contains(PrivateMemory))
        PrivateMemory
      else
        throw UnexpectedAddressSpaceException("No suitable address space found!!")
    } else
      writeTo
  }


  private def inferFunCallWriteTo2(writeTo : OpenCLAddressSpace, args: Seq[OpenCLAddressSpace]) = {
    if (writeTo == UndefAddressSpace) {

      if (args.size == 1)
        args.head
      else {
        AddressSpaceCollection(args).findCommonAddressSpace()
      }
    }
    else
      writeTo

  }

  private def setAddressSpace(e: Expr, writeTo : OpenCLAddressSpace = UndefAddressSpace) : OpenCLAddressSpace = {
    val retAS = e match {
      case Value(_) =>

//        if (writeTo != PrivateMemory && writeTo != UndefAddressSpace)
//          throw UnexpectedAddressSpaceException(
//            s"Value $e cannot have address space $writeTo")

        // note that the address space of a Value may be already set to Private
        // (the same Value can appear multiple times in the IR graph)
        addAddressSpace(e, PrivateMemory)

      case vp: VectorParam =>
        vp.p.addressSpaces

      case p: Param =>
        if (p.addressSpaces == UndefAddressSpace)
          throw UnexpectedAddressSpaceException(s"$p has no address space")
        p.addressSpaces

      case f: FunCall =>
        setAddressSpace(f, writeTo)
    }

    e.addressSpaces = retAS
    retAS
  }

  private def setAddressSpace(call: FunCall,
    writeTo: OpenCLAddressSpace) : OpenCLAddressSpace = {

    val retAS =
      call.f match {

        case Unzip() | Zip(_) | Transpose() | TransposeW() | asVector(_) |
             asScalar() | Split(_) | Join() | Scatter(_) | Gather(_) |
             Pad(_, _) | Tuple(_) | Group(_) | Head() | Tail() =>

          // Pass through
          val s = call.args.map(setAddressSpace(_, writeTo))

          if  (s.length == 1)
            s.head
          else AddressSpaceCollection(s)

        case Filter() =>

          val s = call.args.map(setAddressSpace(_, writeTo))
          s(0)

        case Get(i) =>
          val argspaces = call.args.map(setAddressSpace(_, writeTo))

          argspaces.head match {
            case collection: AddressSpaceCollection => collection.spaces(i)
            case _ => argspaces.head
          }

        case _ =>

          val addressSpaces = call.args.map(setAddressSpace(_, writeTo))
          call.f match {

            case t: toLocal =>

              if (!call.isConcrete(false))
                throw UnexpectedAddressSpaceException("toLocal that doesn't write to memory")

              setAddressSpaceLambda(t.f, LocalMemory, addressSpaces)

            case t: toPrivate =>

              if (!call.isConcrete(false))
                throw UnexpectedAddressSpaceException("toPrivate that doesn't write to memory")

              setAddressSpaceLambda(t.f, PrivateMemory, addressSpaces)

            case t: toGlobal =>

              if (!call.isConcrete(false))
                throw UnexpectedAddressSpaceException("toGlobal that doesn't write to memory")

              setAddressSpaceLambda(t.f, GlobalMemory, addressSpaces)

            case r: AbstractPartRed =>
              // first argument is initial value
              if (call.args(0).addressSpaces == UndefAddressSpace)
                throw UnexpectedAddressSpaceException(
                  s"No address space ${call.args(0).addressSpaces} at $call")

              // the address space of the result of a reduction
              // is always the same as the initial element
              val writeTo = call.args(0).addressSpaces

              setAddressSpaceLambda(r.f, writeTo, addressSpaces)

            case l: Lambda =>
              setAddressSpaceLambda(l, writeTo, addressSpaces)

            case fp: FPattern =>
              setAddressSpaceLambda(fp.f, writeTo, addressSpaces)

            case VectorizeUserFun(_, _) | UserFun(_, _, _, _, _) =>
              val inferredWriteTo = inferFunCallWriteTo2(writeTo, addressSpaces)

              addAddressSpace(call, inferredWriteTo)


          }
      }

    retAS
  }


}
