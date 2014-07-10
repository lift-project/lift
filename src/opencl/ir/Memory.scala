package opencl.ir

import scala.collection.mutable

import ir._
import opencl.ir.toLocal

abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString() =  "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString() =  "global"
}

object UndefAddressSpace extends OpenCLAddressSpace

case class OpenCLMemory(variable: Var, size: Expr, t: Type, addressSpace: OpenCLAddressSpace) extends Memory {
  override def toString() = "(" + variable + ", " + ExprSimplifier.simplify(size) + ", " + t + ", " + addressSpace + ")"
}

class OpenCLSubMemory(parent: OpenCLMemory, size: Expr, t: Type, accessFun: (Expr) => Expr) extends OpenCLMemory(parent.variable, size, t, parent.addressSpace)

// Allocate memory globally
class GlobalAllocator {
  val mems = mutable.Set[OpenCLMemory]()

  def alloc(f: Fun, numGlb: Expr = 1, numLcl: Expr = 0, inputMem : OpenCLMemory = OpenCLNullMemory, outputMem : OpenCLMemory = OpenCLNullMemory) : OpenCLMemory = {

    // allocate input memory if needed (should only be for top-level function)
    val inMem =
      if (inputMem == OpenCLNullMemory) {
        f match {
          case i: Input => OpenCLNullMemory
          case _ => OpenCLMemory(Var(ContinousRange(Cst(0), Type.getSizeInBytes(f.inT))), Type.getSizeInBytes(f.inT), f.inT, GlobalMemory)
        }

     }
     else
      inputMem


    val glbOutSize = Type.getSizeInBytes(f.ouT)*numGlb
    val lclOutSize = Type.getSizeInBytes(f.ouT)*numLcl

    // allocate output memory if force to
    val outMem = f match {

      case Input =>
        assert(outputMem == OpenCLNullMemory)
        OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, f.ouT, GlobalMemory)

      case tg: toGlobal if (outputMem.addressSpace != GlobalMemory) =>
        OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, f.ouT, GlobalMemory)
      case tl: toLocal if (outputMem.addressSpace != LocalMemory) =>
        OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, f.ouT, LocalMemory)

      case _ => outputMem

    }

    // recursive call
    val len = Type.getLength(f.ouT)
    f match {

      case m: MapGlb | MapWrg => alloc(m.f, numGlb*len, numLcl, inMem, outMem)
      case m: MapLcl | MapWarp | MapLane => alloc(m.f, numGlb*len, numLcl*len, inMem, outMem)

      case cf: CompFun => outMem // TODO cf.funs.foldRight(inputMem)((f,mem) => alloc(f, mem, NullMemory))

      case i : Iterate => outMem // TODO

      case fp: FPattern => alloc(fp.f, numGlb, numLcl, inMem, outMem)

      case _ => if (outMem == OpenCLNullMemory) {
        inMem.addressSpace match {
          case GlobalMemory => OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, f.ouT, GlobalMemory)
          case LocalMemory => OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, f.ouT, LocalMemory)
        }
        else
          outMem
      }




    }


  }

}

object OpenCLNullMemory extends OpenCLMemory(Var("NULL"), Cst(0), UndefType, UndefAddressSpace)

object OpenCLMemory {
  
  def asOpenCLMemory(m : Memory) : OpenCLMemory = {
    m match {
      case oclm : OpenCLMemory => oclm
      case NullMemory => OpenCLNullMemory
      case _ => throw new IllegalArgumentException
    }
  }

  private def alloc(f: Fun, inputMem : Memory = NullMemory, outputMem : Memory = NullMemory) : Memory = {

    val size = Type.getSizeInBytes(f.ouT)

    val outMem = f match {

      case cf: CompFun => cf.funs.foldRight(inputMem)((f,mem) => alloc(f, mem, NullMemory))

      case tl: toLocal => {
        val localMem = OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, f.ouT, LocalMemory)
        alloc(tl.f, inputMem, localMem)
        localMem
      }
      case tg: toLocal => {
        val globalMem = OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, f.ouT, GlobalMemory)
        alloc(tg.f, inputMem, globalMem)
        globalMem
      }

      case in: Input =>
        assert (inputMem == NullMemory)
        OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, in.ouT, GlobalMemory) // global address space for all inputs

      case i : Iterate => {

      }

      // TODO: create a view of the memory or sub-memory in case of Reorder

      case _ =>
        val outMem =
          if (outputMem == NullMemory)
            OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, f.ouT, asOpenCLMemory(inputMem).addressSpace) // same address space as the input
          else
            outputMem
        f match {
          case fp: FPattern => alloc(fp.f, outMem)
          case _ =>
        }
        outMem
    }

    assert (outMem != NullMemory)

    outMem
  }


/*
  def allocate(f: Fun, inputMem: Memory = NullMemory): Array[Memory] = {


    f.memory = f match {
      
      case cf: CompFun => {
        // only pass the output of the previous one as input to the next
        cf.funs.foldRight(Array(inputMem))((f, mem) => {
          val (init, last) =  if (mem.nonEmpty) { (mem.init, mem.last) }
                              else { (Array.empty[Memory], NullMemory) }
          init ++ allocate(f, last)
        })
      }
      
      case in : Input => {
        assert(inputMem == NullMemory)
        val size = Type.getSizeInBytes(in.ouT)
        Array(OpenCLMemory(in.variable, size, in.ouT, GlobalMemory))
      }
      
      // the sequential implementations allocate new memory objects
      case MapSeq(_) | ReduceSeq(_) | ReduceHost(_) => {
        val size = Type.getSizeInBytes(f.ouT)
        val addressSpace = asOpenCLMemory(inputMem).addressSpace
        Array(inputMem, OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, f.ouT, addressSpace))
      }

      // The other map implementations multiply the sizes
      case m : AbstractMap => {
        val len = Type.getLength(m.ouT)

        // get the newly allocated mem objects without the inputMem
        val mems = allocate(m.f, inputMem).drop(1)

        // keep inputMem untouched &
        // multiply each of the newly allocated mem objects with the length for this map
        Array(inputMem) ++ mems.map( (mem) => {
          multiplyLength(m, asOpenCLMemory(mem), len)
        })
      }

      case i : Iterate => {
        // get the newly allocated mem objects without the inputMem
        Array(inputMem) ++ allocate(i.f, inputMem).drop(1)
      }

      case tL : toLocal => {
        val mems = allocate(tL.f, inputMem)
        // get the newest allocated mem object (the current "output")
        val oldOutput = asOpenCLMemory(mems.last)
        val newOutput = OpenCLMemory(oldOutput.variable, oldOutput.size, oldOutput.t, LocalMemory)
        // change the AddressSpace of the output and update it in the nested function
        updateOutput(tL.f, newOutput)
        mems.init :+ newOutput
      }

      case tG : toGlobal => {
        val mems = allocate(tG.f, inputMem)
        // get the newest allocated mem object (the current "output")
        val oldOutput = asOpenCLMemory(mems.last)
        val newOutput = OpenCLMemory(oldOutput.variable, oldOutput.size, oldOutput.t, GlobalMemory)
        // change the AddressSpace of the output and update it in the nested function
        updateOutput(tG.f, newOutput)
        mems.init :+ newOutput
      }

      case _ => Array(inputMem)
    }

    f.memory
  }

  // update the last memory object recursively in nested Funs
  def updateOutput(f: Fun, output: Memory) : Unit = {
    if (f.memory.nonEmpty) {
      f.memory = f.memory.init :+ output

      f match {
        case fp: FPattern => {
          updateOutput(fp.f, output)
        }
        case _ => ;
      }
    }
  }

  def multiplyLength(m: AbstractMap, mem: OpenCLMemory, len: Expr): Memory = {
    val addressSpace = mem.addressSpace
    // check combination of map and the address space ...
    (m, addressSpace) match {
      case (_:MapWrg, LocalMemory) => mem // .. do nothing for local memory when inside the map wrg ...
      // ... but multiply by default
      case _ => {
        mem.variable.updateRange( (r) => { r * len })
        OpenCLMemory(mem.variable, ExprSimplifier.simplify(mem.size * len), mem.t, addressSpace)
      }
    }
  }*/
  
}
