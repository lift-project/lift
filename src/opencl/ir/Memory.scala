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

case class OpenCLMemory(variable: Var, size: Expr, t: Type, addressSpace: OpenCLAddressSpace) extends Memory {
  override def toString() = "(" + variable + ", " + Expr.simplify(size) + ", " + t + ", " + addressSpace + ")"
}

object OpenCLMemory {
  
  def asOpenCLMemory(m : Memory) : OpenCLMemory = {
    m match {
      case oclm : OpenCLMemory => oclm
      case _ => throw new IllegalArgumentException
    }
  }
  
  def allocate(f: Fun) : Array[Memory] = {
    allocate(f, Array.empty[Memory])
  }

  def allocate(f: Fun, memory: Array[Memory]): Array[Memory] = {

    f.memory = f match {
      
      case cf: CompFun => {
        // only pass the output of the previous one as input to the next
        cf.funs.foldRight(memory)((f, mem) => {
          val (init, last) =  if (mem.nonEmpty) { (mem.init, mem.takeRight(1)) }
                              else { (Array.empty[Memory], Array.empty[Memory]) }
          init ++ allocate(f, last)
        })
      }
      
      // inputs (parameters) are always allocated in global memory in OpenCL
      case in : Input => {
        val size = Type.getSizeInBytes(in.ouT)
        memory :+ OpenCLMemory(in.variable, size, in.ouT, GlobalMemory)
      }
      
      // the sequential implementations allocate new memory objects
      case r : ReduceSeq => {
        val size = Type.getSizeInBytes(r.ouT)
        val addressSpace = asOpenCLMemory(memory.head).addressSpace
        memory :+ OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, r.ouT, addressSpace)
      }

      case m : MapSeq => {
        val size = Type.getSizeInBytes(m.ouT)
        val addressSpace = asOpenCLMemory(memory.head).addressSpace
        memory :+ OpenCLMemory(Var(ContinousRange(Cst(0), size)), size, m.ouT, addressSpace)
      }

      // The other map implementations multiply the sizes
      case m : AbstractMap => {
        val len = Type.getLength(m.ouT)

        // get the newly allocated mem objects
        val mems = allocate(m.f, memory).drop(memory.length)

        // multiply each of the newly allocated mem objects with the length for this map
        memory ++ mems.map( (mem) => {
          val addressSpace = asOpenCLMemory(mem).addressSpace
          val size = mem.size
          OpenCLMemory(mem.variable, Expr.simplify(size * len), mem.t, addressSpace)
        })
      }

      case i : Iterate => {
        allocate(i.f, memory)
      }

      case tL : toLocal => {
        val mems = allocate(tL.f, memory)
        // get the newest allocated mem object
        val last = asOpenCLMemory(mems.last)
        // change the AddressSpace of the last mem object
        mems.init :+ OpenCLMemory(last.variable, last.size, last.t, LocalMemory)
      }

      case tG : toGlobal => {
        val mems = allocate(tG.f, memory)
        // get the newest allocated mem object
        val last = asOpenCLMemory(mems.last)
        // change the AddressSpace of the last mem object
        mems.init :+ OpenCLMemory(last.variable, last.size, last.t, GlobalMemory)
      }

      case _ => memory
    }
    f.memory
  }
  
}
