package opencl.ir

import ir._


//MapWgr declar 
//	MapLcl
//		ReduceSeq

abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString() =  "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString() =  "global"
}

object UndefAddressSpace extends OpenCLAddressSpace

case class OpenCLMemory(variable: Var, size: Expr, t: Type, addressSpace: OpenCLAddressSpace) extends Memory {
  override def toString() = "(" + variable + ", " + Expr.simplify(size) + ", " + t + ", " + addressSpace + ")"
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
      
      // inputs (parameters) are always allocated in global memory in OpenCL
      case in : Input => {
        assert(inputMem == NullMemory)
        val size = Type.getSizeInBytes(in.ouT)
        Array(OpenCLMemory(in.variable, size, in.ouT, GlobalMemory))
      }
      
      // the sequential implementations allocate new memory objects
      case r : ReduceSeq => {
        val size = Type.getSizeInBytes(r.ouT)
        val addressSpace = asOpenCLMemory(inputMem).addressSpace
        Array(inputMem, OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, r.ouT, addressSpace))
      }

      case m : MapSeq => {
        val size = Type.getSizeInBytes(m.ouT)
        val addressSpace = asOpenCLMemory(inputMem).addressSpace
        Array(inputMem, OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, m.ouT, addressSpace))
      }

      // The other map implementations multiply the sizes
      case m : AbstractMap => {
        val len = Type.getLength(m.ouT)

        // get the newly allocated mem objects without the inputMem
        val mems = allocate(m.f, inputMem).drop(1)

        // keep inputMem untouched &
        // multiply each of the newly allocated mem objects with the length for this map
        Array(inputMem) ++ mems.map( (mem) => {
          val addressSpace = asOpenCLMemory(mem).addressSpace
          val size = mem.size
          OpenCLMemory(mem.variable, Expr.simplify(size * len), mem.t, addressSpace)
        })
      }

      case i : Iterate => {
        // get the newly allocated mem objects without the inputMem
        val mems = allocate(i.f, inputMem).drop(1)

        // iterate does double buffering ...
        // ... doublicate last mem object
        val last = mems.last

        Array(inputMem) ++ mems :+ last
      }

      case tL : toLocal => {
        val mems = allocate(tL.f, inputMem)
        // get the newest allocated mem object
        val last = asOpenCLMemory(mems.last)
        // change the AddressSpace of the last mem object
        mems.init :+ OpenCLMemory(last.variable, last.size, last.t, LocalMemory)
      }

      case tG : toGlobal => {
        val mems = allocate(tG.f, inputMem)
        // get the newest allocated mem object
        val last = asOpenCLMemory(mems.last)
        // change the AddressSpace of the last mem object
        mems.init :+ OpenCLMemory(last.variable, last.size, last.t, GlobalMemory)
      }

      case _ => Array(inputMem)
    }

    f.memory
  }
  
}
