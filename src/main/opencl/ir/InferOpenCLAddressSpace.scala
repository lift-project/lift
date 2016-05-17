package opencl.ir

import ir.ast.{AbstractPartRed, Expr, FPattern, Filter, FunCall, Gather, Get, Group, Head, IRNode, Join, Lambda, Pad, Param, Scatter, Split, Tail, Transpose, TransposeW, Tuple, Unzip, UserFun, Value, VectorizeUserFun, Zip, asScalar, asVector}
import opencl.ir.pattern.{toGlobal, toLocal, toPrivate}

/**
  * Created by Toomas Remmelg on 17/05/16.
  */
object InferOpenCLAddressSpace {

  private def addAddressSpace(e: Expr, as : OpenCLAddressSpace) = {
    e.addressSpaces += as
    e.addressSpaces
  }

  def apply(lambda: Lambda) =
    setAddressSpace(lambda)

  // by default the parameters of the lambda will be set to global space
  // and the return address space should be in global memory
  def setAddressSpace(l: Lambda) : Unit = {

    //new DotPrinter(new PrintWriter(new File("/home/cdubach/graph1.dot")), false, false,true).print(l)

    // clear up all the address spaces first
    IRNode.visit(l, _ match {
        case e: Expr => e.addressSpaces.clear
        case _ =>
      }
    )

    // set the param address space to global memory
    l.params.foreach(p => addAddressSpace(p, GlobalMemory))
    setAddressSpace(l.body)

    //new DotPrinter(new PrintWriter(new File("/home/cdubach/graph.dot")), false, true).print(l)


    if (l.body.addressSpaces.size != 1 | ! l.body.addressSpaces.contains(GlobalMemory) )
      throw UnexpectedAddressSpaceException(
        l.body.addressSpaces.toString,
        GlobalMemory.toString
      )
  }

  private def setAddressSpaceLambda(l: Lambda, writeTo : OpenCLAddressSpace, argsAddrSpace : Seq[Set[OpenCLAddressSpace]]) = {
    l.params.zip(argsAddrSpace).foreach({case (p,a) =>
      assert (p.addressSpaces.isEmpty)
      p.addressSpaces ++= a
    })
    setAddressSpace(l.body, writeTo)
  }

  private def inferFunCallWriteTo(writeTo : OpenCLAddressSpace, args: Set[Expr]) = {
    if (writeTo == UndefAddressSpace) {
      val addSpaces = args.map(_.addressSpaces).reduce(_ ++ _)
      if (addSpaces.contains(GlobalMemory))
        GlobalMemory
      else if (addSpaces.contains(LocalMemory))
        LocalMemory
      else if (addSpaces.contains(PrivateMemory))
        PrivateMemory
      else
        UndefAddressSpace
    } else
      writeTo
  }

  private def setAddressSpace(e: Expr, writeTo : OpenCLAddressSpace = UndefAddressSpace) : scala.collection.Set[OpenCLAddressSpace] = {
     val retAS = e match {
      case Value(_) =>

        if (writeTo != PrivateMemory && writeTo != UndefAddressSpace)
          throw UnexpectedAddressSpaceException(
            s"Value $e cannot have address space $writeTo")

        // note that the address space of a Value may be already set to Private
        // (the same Value can appear multiple times in the IR graph)
        addAddressSpace(e, PrivateMemory)

      case p: Param =>
        assert (p.addressSpaces.nonEmpty)
        p.addressSpaces

      case f: FunCall =>
        f.args.foreach(setAddressSpace(_))
        val addressSpaces = f.args.map(_.addressSpaces.toSet)
        f.f match {
          case l: Lambda =>
            addAddressSpace(f, writeTo)
            setAddressSpaceLambda(l, writeTo, addressSpaces)

          case t: toLocal =>
            addAddressSpace(f, LocalMemory)
            setAddressSpaceLambda(t.f, LocalMemory, addressSpaces)

          case t: toPrivate =>
            addAddressSpace(f, PrivateMemory)
            setAddressSpaceLambda(t.f, PrivateMemory, addressSpaces)

          case t: toGlobal =>
            addAddressSpace(f, GlobalMemory)
            setAddressSpaceLambda(t.f, GlobalMemory, addressSpaces)

          case r: AbstractPartRed =>
            // first argument is initial value
            assert (f.args(0).addressSpaces.size == 1)

            // the address space of the result of a reduction
            // is always the same as the initial element
            val writeTo = f.args(0).addressSpaces.toList(0)

            addAddressSpace(f, writeTo)
            setAddressSpaceLambda(r.f, writeTo, addressSpaces)

          case fp : FPattern =>
            val inferredWriteTo = inferFunCallWriteTo(writeTo, f.args.toSet)
            addAddressSpace(f, inferredWriteTo)
            setAddressSpaceLambda(fp.f, inferredWriteTo, addressSpaces)

          case VectorizeUserFun(_,_) | UserFun(_, _, _, _, _) =>
            val inferredWriteTo = inferFunCallWriteTo(writeTo, f.args.toSet)
            assert (e.addressSpaces.isEmpty)
            addAddressSpace(f, inferredWriteTo)

          case Unzip() | Zip(_) | Transpose() | TransposeW() | asVector(_) |
               asScalar() | Split(_) | Join() | Scatter(_) | Gather(_) |
               Pad(_, _) | Tuple(_) | Group(_) | Filter() | Head() | Tail() |
               Get(_) =>

            val inferredWriteTo = inferFunCallWriteTo(writeTo, f.args.toSet)
            assert (e.addressSpaces.isEmpty)
            addAddressSpace(f, inferredWriteTo)
        }

    }

    retAS
  }


}
