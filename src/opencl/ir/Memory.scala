package opencl.ir

import ir._

abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString = "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString = "global"
}

object UndefAddressSpace extends OpenCLAddressSpace

case class OpenCLMemory(variable: Var, size: Expr, t: Type, addressSpace: OpenCLAddressSpace) extends Memory {

  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new IllegalArgumentException

  override def toString = "(" + variable + ", " + ExprSimplifier.simplify(size) + ", " + t + ", " + addressSpace + ")"
}

class OpenCLSubMemory(parent: OpenCLMemory, size: Expr, t: Type, accessFun: (Expr) => Expr) extends OpenCLMemory(parent.variable, size, t, parent.addressSpace)

// Allocate memory globally
class GlobalAllocator {


}

object OpenCLNullMemory extends OpenCLMemory(Var("NULL"), Cst(0), UndefType, UndefAddressSpace)

object OpenCLMemory {

  def getMaxSizeInBytes(t: Type) : Expr = {
    Expr.max(getSizeInBytes(t))
  }

  private def getSizeInBytes(t: Type) : Expr = {
    ExprSimplifier.simplify(
      t match {
        case st: ScalarType => st.size
        case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
        case at: ArrayType => at.len * getSizeInBytes(at.elemT)
        case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
        case _ => throw new TypeException(t, "??")
      }
    )
  }

  def asOpenCLMemory(m : Memory) : OpenCLMemory = {
    m match {
      case oclm : OpenCLMemory => oclm
      case UnallocatedMemory => OpenCLNullMemory
      case _ => throw new IllegalArgumentException
    }
  }


  // allocate input memory if needed (should only be for top-level function)
  def fixInput(f: Fun, inputMem : OpenCLMemory) : OpenCLMemory = {
    if (inputMem == OpenCLNullMemory) {
      f match {
        case _: Input => OpenCLNullMemory
        case _: CompFun => OpenCLNullMemory
        case _ => OpenCLMemory(Var(ContinousRange(Cst(0), getMaxSizeInBytes(f.inT))), getMaxSizeInBytes(f.inT), f.inT, GlobalMemory)
      }
    }
    else
      inputMem
  }

  // allocate an output memory based on the input memory
  def allocOutput(glbOutSize: Expr, lclOutSize: Expr, inputMem: OpenCLMemory, ouputType: Type) : OpenCLMemory = {
    assert (inputMem != OpenCLNullMemory)

    inputMem.addressSpace match {
      case GlobalMemory => OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, ouputType, GlobalMemory)
      case LocalMemory => OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, ouputType, LocalMemory)
    }
  }

  def alloc(f: Fun, numGlb: Expr = 1, numLcl: Expr = 0, argInMem : OpenCLMemory = OpenCLNullMemory, outputMem : OpenCLMemory = OpenCLNullMemory) : OpenCLMemory = {

    // fix in the input memory if needed
    val inMem = fixInput(f, argInMem)
    assert(inMem != OpenCLNullMemory || f.isInstanceOf[Input] || f.isInstanceOf[CompFun])
    f.inM = inMem

    val glbOutSize = getMaxSizeInBytes(f.ouT) * numGlb
    val lclOutSize = getMaxSizeInBytes(f.ouT) * numLcl

    val len = Type.getLength(f.ouT)
    val result : OpenCLMemory = f match {

      case Input(_,_) =>
        assert(outputMem == OpenCLNullMemory)
        OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, f.ouT, GlobalMemory)

      case MapGlb(_) | MapWrg(_) => alloc(f.asInstanceOf[AbstractMap].f, numGlb * len, numLcl, inMem, outputMem)
      case MapLcl(_) | MapWarp(_) | MapLane(_) => alloc(f.asInstanceOf[AbstractMap].f, numGlb * len, numLcl * len, inMem, outputMem)

      case tg: toGlobal =>
        val mem =
          if (outputMem == OpenCLNullMemory || outputMem.addressSpace != GlobalMemory)
            OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, f.ouT, GlobalMemory)
        else
            outputMem
        alloc(tg.f, numGlb, numLcl, inMem, mem)

      case tl: toLocal =>
        val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != LocalMemory)
          OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, f.ouT, LocalMemory)
        else
          outputMem
        alloc(tl.f, numGlb, numLcl, inMem, mem)

      case cf: CompFun =>
        cf.funs.foldRight(inMem)((f,mem) => alloc(f, numGlb, numLcl, mem))

      case it: Iterate =>

        val inSize = getMaxSizeInBytes(it.inT)
        val outSize = getMaxSizeInBytes(it.ouT)
        val diff = ExprSimplifier.simplify(inSize - outSize)
        val largestSize =  diff match {
          case Cst(c) => if (c < 0) outSize else inSize
          case _ => throw new IllegalArgumentException // maybe bug in expression simplifier??
        }

        // create a swap buffer
        it.swapBuffer = OpenCLMemory(Var(ContinousRange(Cst(0), largestSize)), largestSize, UndefType, inMem.addressSpace)

        alloc(it.f, numGlb, numLcl, inMem)

      case fp: FPattern => alloc(fp.f, numGlb, numLcl, inMem, outputMem)

      // some function do not create anything, TODO: add toVector, ToScalar, reorder,...
      case Split(_) | Join() => inMem

      case _ =>
        if (outputMem == OpenCLNullMemory)
          allocOutput(glbOutSize, lclOutSize, inMem, f.ouT)
        else
          outputMem
    }

    f.outM = result

    result

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
