package backends.c.common.memory_management

import backends.c.common.common_ir.CPUMainMemoryAddressSpace
import backends.c.host.host_ir._
import ir.ast.{Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Expr, FPattern, FunCall, FunDecl, IRNode, Lambda, Pattern, UserFun}
import opencl.ir.{GlobalMemory, OpenCLAddressSpace, UndefAddressSpace}

object InferHostMemoryAddressSpace {

  private def inferAddrSpace(node: IRNode): Unit = {
    node match {
      //case fc@FunCall(_:UserFun,_*) => inferAddrSpaceUserFunc(fc)

      case fc@FunCall(_:ToGPU, arg) =>
        inferAddrSpace(arg)
        assert(arg.addressSpace == CPUMainMemoryAddressSpace)
        fc.addressSpace = GlobalMemory

      case fc@FunCall(_:ToHost, arg) =>
        inferAddrSpace(arg)
        assert(arg.addressSpace == GlobalMemory)
        fc.addressSpace = CPUMainMemoryAddressSpace

      case fc@FunCall(_:OclFunc, args@_*) =>
        args.foreach(inferAddrSpace(_))
        fc.addressSpace = GlobalMemory

      case fc@FunCall(_:CPUFunc, args@_*) =>
        args.foreach(inferAddrSpace(_))
        fc.addressSpace = CPUMainMemoryAddressSpace

      case fc@FunCall(fp:FPattern, args@_*) => {
        args.foreach(inferAddrSpace(_))

        //let other args address space to be one of the known address space,
        //like ReduceSeq can take a value as input, thus its space may not be inferred when entering here
        var addressSpace:OpenCLAddressSpace = UndefAddressSpace
        args.foreach(a => a.addressSpace match {
          case UndefAddressSpace =>
          case _ => addressSpace = a.addressSpace
        })
        assert(addressSpace != UndefAddressSpace, "All args's address space is unknown!")
        args.foreach(_.addressSpace = addressSpace)

        (fp.f.params zip args).foreach(pair => pair._1.addressSpace = pair._2.addressSpace)
        inferAddrSpace(fp.f.body)

        //fc.addressSpace = args.head.addressSpace
        fc.addressSpace = fp.f.body.addressSpace
      }
      case fc@FunCall(p:Pattern, args@_*) => {
        args.foreach(inferAddrSpace(_))
        assert(args.head.addressSpace != UndefAddressSpace)
        args.foreach(a=>assert(a.addressSpace == args.head.addressSpace))
        fc.addressSpace = args.head.addressSpace
      }

      case fc@FunCall(Lambda(params, body, _), args@_*) => {
        args.foreach(inferAddrSpace(_))

        (params zip args).foreach(pair => pair._1.addressSpace = pair._2.addressSpace)
        inferAddrSpace(body)

        fc.addressSpace = args.head.addressSpace
      }
      case fc@FunCall(_:UserFun, args@_*) => {
        //in case user func args has non-trivial construct like: Get(0)
        args.foreach(inferAddrSpace(_))

        //let other args address space to be one of the known address space
        var addressSpace:OpenCLAddressSpace = UndefAddressSpace
        args.foreach(a => a.addressSpace match {
          case UndefAddressSpace =>
          case _ => addressSpace = a.addressSpace
        })
        assert(addressSpace != UndefAddressSpace, "All args's address space is unknown!")
        args.foreach(_.addressSpace = addressSpace)

        fc.addressSpace = args.head.addressSpace
      }
      case FunCall(_:FunDecl, _) => assert(false)

      //ad-hoc, just for understanding
      //maybe not too bad, in host code generator, the memory is always CPU,
      //For GPU case, you pass to GPU generator, the memory will be correct as well.
      case a1d:ArrayFromUserFunGenerator =>
        a1d.addressSpace = CPUMainMemoryAddressSpace
      case a3d:Array3DFromUserFunGenerator =>
        a3d.addressSpace = CPUMainMemoryAddressSpace

      case _ =>
    }
  }

  def apply(lambda: Lambda): Unit = {
    //reset the address space, in case we rewrite some expression that is already compiled,
    //and get some weird side effects.
    lambda visitBy {
      case e:Expr => e.addressSpace = UndefAddressSpace
      case _ =>
    }


    //assert that all memory space has not been inferred
    lambda visitBy {
      case e:Expr =>
        assert( e.addressSpace == UndefAddressSpace )
      case _ =>
    }

    //alloc for the starting params
    lambda.params.foreach(_.addressSpace = CPUMainMemoryAddressSpace)

    inferAddrSpace(lambda.body)

    //assert that all memory space has been inferred
    lambda visitBy {
      case e:Expr => assert( e.addressSpace != UndefAddressSpace )
      case _ =>
    }
  }


  }
