package cbackends.common.memory_management

import core.generator.GenericAST.CVarWithType
import cbackends.common.common_ir.{CPUNullMemory, HostMemory, HostMemoryCollection}
import cbackends.host.host_ir._
import ir.ast.{AbstractMap, AbstractPartRed, ArrayConstructors, Expr, FPattern, FunCall, FunDecl, Get, IRNode, Join, Lambda, Pad, Slide, Split, Transpose, TransposeW, UserFun, Value, Zip}
import ir.{Type, UnallocatedMemory}
import lift.arithmetic.{ArithExpr, ContinuousRange, Cst, Var}
import opencl.ir.OpenCLMemory

import scala.collection.mutable


object MemoryAllocator {

  def alloc(node:IRNode): Unit = {
    node match {

      case v:Value =>
        v.mem = CPUNullMemory

      case ac:ArrayConstructors =>
        //ac.mem = CPUNullMemory
        val size = Type.getAllocatedSize(ac.t)
        //allocate memory in IR, but not included in hostMemory, so that no mem allocation code is emitted for param,
        //but the IR analysis can still be done.
        ac.mem = HostMemory(Var(s"array_constructor_${ac.gid}", ContinuousRange(Cst(0), size)), size, ac.addressSpace )

      case fc@FunCall(_:ToGPU|_:OclFunCall, arg) =>
        alloc(arg)

        val size = Type.getElementCount(fc.t)
        fc.mem = OpenCLMemory(Var(s"user_func_${fc.gid}", ContinuousRange(Cst(0), size)), size, fc.addressSpace )

      case fc@FunCall(_:UserFun|_:CPUFunCall|_:CPUFunCall2|_:ToHost, args@_*) => {
        //link the arg to the correct param is already done in its upper level FPattern
        args.foreach(alloc(_))

        val size = Type.getElementCount(fc.t)
        fc.mem = HostMemory(Var(s"user_func_${fc.gid}", ContinuousRange(Cst(0), size)), size, fc.addressSpace )

      }

      case fc@FunCall(rd:AbstractPartRed, args@_*) => {

        assert(args.length == 2)
        val init = args(0)
        val array = args(1)

        alloc(array)
        init.mem = CPUNullMemory

        (rd.f.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        alloc(rd.f.body)

        fc.mem = rd.f.body.mem

      }

      case fc@FunCall(fp:FPattern, args@_*) => {

        args.foreach( alloc(_) )

        (fp.f.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        alloc(fp.f.body)

        fc.mem = fp.f.body.mem

      }
      case fc@FunCall(_:Zip, args@_*) => {

        args.foreach(alloc(_))

        fc.mem = HostMemoryCollection( args.map(_.mem.asInstanceOf[HostMemory]) )

      }

      case fc@FunCall(l:Lambda, args@_*) => {

        args.foreach( alloc(_) )

        (l.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        alloc(l.body)

        fc.mem = l.body.mem
      }

        //for Slide etc.
      case fc@FunCall(_:Join|_:Slide|_:Zip|_:Get|_:Split|_:Join|_:Transpose|_:TransposeW|_:Pad, arg) => {
        alloc(arg)
        fc.mem = arg.mem
      }


      case x:Expr if x.mem == UnallocatedMemory =>
        assert(false)

      case _ =>



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

  def apply(lambda: Lambda): Unit = {


    pre_check(lambda)

    //alloc params
    lambda.params.foreach(
      p => {
        val size = Type.getAllocatedSize(p.t)
        //allocate memory in IR, but not included in hostMemory, so that no mem allocation code is emitted for param,
        //but the IR analysis can still be done.
        p.mem = HostMemory(Var(s"initial_param_${p.gid}", ContinuousRange(Cst(0), size)), size, p.addressSpace )
      }
    )

    alloc(lambda.body)

    post_check(lambda)

  }

}
