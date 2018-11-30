package host.memory_management

import core.generator.GenericAST.CVarWithType
import host.ir_host.{CPUNullMemory, HostMemory, HostMemoryCollection}
import host.lowering.Util
import ir.{Type, UnallocatedMemory}
import ir.ast.{AbstractMap, AbstractPartRed, ArrayConstructors, Expr, FPattern, FunCall, FunDecl, IRNode, Lambda, UserFun, Value, Zip}
import lift.arithmetic.{ArithExpr, ContinuousRange, Cst, Var}

import scala.collection.mutable


object MemoryAllocator {

  var hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr) ]

  def alloc(node:IRNode): Unit = {
    node match {

      case ac:ArrayConstructors =>
        ac.mem = CPUNullMemory

      case fc@FunCall(_:UserFun, args@_*) => {
        //link the arg to the correct param is already done in its upper level FPattern
        args.foreach(alloc(_))

        val size = Type.getAllocatedSize(fc.t)
        fc.mem = HostMemory(Var(s"user_func_${fc.gid}", ContinuousRange(Cst(0), size)), size, fc.addressSpace )

        hostMemoryDeclaredInSignature +=  fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, Util.Array2Pointer( Util.IRType2CastType(fc.t), true ) ),  size )

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
        //val size = Type.getAllocatedSize(fc.t)
        //fc.mem = HostMemory(Var(s"reduce_${fc.gid}", ContinuousRange(Cst(0), size)), size, fc.addressSpace )
        //PropagateMemForReduce(fc)

        //correct type for user function, e.g., float => [float]_1
        hostMemoryDeclaredInSignature += fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, Util.Array2Pointer( Util.IRType2CastType(fc.t), true ) ) , Type.getAllocatedSize(fc.t) )

      }

      case fc@FunCall(fp:FPattern, args@_*) => {

        args.foreach( alloc(_) )

        (fp.f.params zip args).foreach(pair => pair._1.mem = pair._2.mem)
        alloc(fp.f.body)

        fc.mem = fp.f.body.mem

        fp match {

          case _:AbstractMap =>
            //here fc.t already have the augmented size information after map, so no need to manually calculate
            hostMemoryDeclaredInSignature += fc.mem.variable.toString -> (CVarWithType(fc.mem.variable.toString, Util.Array2Pointer( Util.IRType2CastType(fc.t), true ) ) , Type.getAllocatedSize(fc.t) )
          case _ =>
        }

      }
      case fc@FunCall(_:Zip, args@_*) => {

        args.foreach(alloc(_))

        fc.mem = HostMemoryCollection( args.map(_.mem.asInstanceOf[HostMemory]) )

      }

      case fc@FunCall(_:FunDecl, arg) => {
        alloc(arg)
        fc.mem = arg.mem
      }

      case _:FunCall => assert(false)

      case _ =>



    }
  }

  def apply(lambda: Lambda): Map[String, (CVarWithType, ArithExpr) ] = {

    lambda visitBy {
      case e:Expr if !e.isInstanceOf[Value] => assert(e.mem ==  UnallocatedMemory )
      case _ =>
    }

    //reset hostMemoryDeclaredInSignature if run with multiple test cases
    hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr) ]

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

    //assert that all memory has been allocated
    lambda visitBy {
        case e:Expr if !e.isInstanceOf[Value] => assert(e.mem !=  UnallocatedMemory )
        case _ =>
      }

    hostMemoryDeclaredInSignature.toMap

  }

}
