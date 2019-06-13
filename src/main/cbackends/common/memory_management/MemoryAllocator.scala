package cbackends.common.memory_management

import core.generator.GenericAST.CVarWithType
import cbackends.common.common_ir.{CPUNullMemory, HostMemory, HostMemoryCollection, Slice}
import cbackends.host.host_ir._
import ir.ast.{AbstractMap, AbstractPartRed, ArrayAccess, ArrayConstructors, Expr, FPattern, FunCall, FunDecl, Get, IRNode, Join, Lambda, Pad, Param, Slide, Split, Transpose, TransposeW, UserFun, Value, Zip}
import ir.{Type, UnallocatedMemory}
import lift.arithmetic.{ArithExpr, ContinuousRange, Cst, Var}
import opencl.ir.OpenCLMemory
import opencl.ir.pattern.ScanSeq

import scala.collection.mutable


object MemoryAllocator {

  def alloc(node:IRNode, cont: IRNode => IRNode): IRNode = {
    node match {

      case p:Param =>
        p

      case v:Value =>
        v.mem = CPUNullMemory; v

      case ac:ArrayConstructors =>
        //ac.mem = CPUNullMemory
        val size = Type.getAllocatedSize(ac.t)
        //allocate memory in IR, but not included in hostMemory, so that no mem allocation code is emitted for param,
        //but the IR analysis can still be done.
        ac.mem = HostMemory(Var(s"array_constructor_${ac.gid}", ContinuousRange(Cst(0), size)), size, ac.addressSpace )
        ac

      case fc@FunCall(_:ToGPU|_:OclFunCall, args@_*) =>
        args.foreach(cont(_))

        val size = Type.getElementCount(fc.t)
        fc.mem = OpenCLMemory(Var(s"user_func_${fc.gid}", ContinuousRange(Cst(0), size)), size, fc.addressSpace )
        fc

      case fc@FunCall(_:UserFun | _:CPUFunCall | _:ToHost, args@_*) => {
        //link the arg to the correct param is already done in its upper level FPattern
        args.foreach(cont(_))

        val size = Type.getElementCount(fc.t)
        fc.mem = HostMemory(Var(s"user_func_${fc.gid}", ContinuousRange(Cst(0), size)), size, fc.addressSpace )
        fc

      }

      case fc@FunCall(rd:AbstractPartRed, args@_*) => {

        assert(args.length == 2)
        val init = args(0)
        val array = args(1)

        cont(array)
        init.mem = CPUNullMemory

        (rd.f.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        cont(rd.f.body)

        fc.mem = rd.f.body.mem
        fc

      }

      case fc@FunCall(s:ScanSeq, args@_*) => {

        assert(args.length == 2)
        val init = args(0)
        val array = args(1)

        cont(array)
        init.mem = CPUNullMemory

        (s.f.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        cont(s.f.body)

        fc.mem = s.f.body.mem
        fc

      }

      case fc@FunCall(fp:FPattern, args@_*) => {

        args.foreach( cont(_) )

        (fp.f.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        cont(fp.f.body)

        fc.mem = fp.f.body.mem

        fc

      }
      case fc@FunCall(_:Zip, args@_*) => {

        args.foreach(cont(_))

        fc.mem = HostMemoryCollection( args.map(_.mem.asInstanceOf[HostMemory]) )

        fc

      }

      case fc@FunCall(l:Lambda, args@_*) => {

        args.foreach( cont(_) )

        (l.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        cont(l.body)

        fc.mem = l.body.mem

        fc
      }

        //for Slide etc.
      case fc@FunCall(_:Join|_:Slide|_:Zip|_:Get|_:Split|_:Join|_:Transpose|_:TransposeW|_:Pad|_:ArrayAccess|_:Slice, arg) => {
        cont(arg)
        fc.mem = arg.mem

        fc
      }


      /*case x:Expr if x.mem == UnallocatedMemory =>
        assert(false)
        x*/

      //case _ =>



    }
  }

  def pre_check(lambda: Lambda) : Unit = {

    lambda visitBy {
      case e:Expr if !e.isInstanceOf[Value] => e.mem =  UnallocatedMemory
      case _ =>
    }

    lambda visitBy {
      case e:Expr if !e.isInstanceOf[Value] => assert(e.mem ==  UnallocatedMemory )
      case _ =>
    }

  }

  def post_check(lambda: Lambda) : Unit = {

    //assert that all memory has been allocated
    lambda visitBy {
      case e:Expr if !e.isInstanceOf[Value] =>
        assert(e.mem !=  UnallocatedMemory )
      case _ =>
    }

  }


  def init_params(lambda: Lambda) : Unit = {

    //alloc params
    lambda.params.foreach(
      p => {
        val size = Type.getAllocatedSize(p.t)
        //allocate memory in IR, but not included in hostMemory, so that no mem allocation code is emitted for param,
        //but the IR analysis can still be done.
        p.mem = HostMemory(Var(s"initial_param_${p.gid}", ContinuousRange(Cst(0), size)), size, p.addressSpace )
      }
    )
  }

  def default_alloc(in: IRNode) : IRNode = {
    alloc(in, default_alloc)
  }

  def apply(lambda: Lambda): Unit = {


    pre_check(lambda)

    init_params(lambda)

    default_alloc(lambda.body)

    post_check(lambda)

  }

}
