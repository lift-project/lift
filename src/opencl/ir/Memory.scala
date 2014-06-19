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

case class OpenCLMemory(variable: Var, size: Expr, t: Type, addressSpace: OpenCLAddressSpace) extends Memory

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
        cf.funs.foldRight(memory)((f, mem) => allocate(f, mem))        
      }
      
      // inputs (parameters) are always allocated in global memory in OpenCL
      case in : Input => {
        val size = Type.getSizeInBytes(in.ouT)
        memory :+ OpenCLMemory(in.variable, size, in.ouT, GlobalMemory)
      }
      
      // the sequential implementations allocates a new memory object
      case rf : ReduceSeq => {
        val size = Type.getSizeInBytes(rf.ouT)
        memory :+ OpenCLMemory(Var(), size, rf.ouT, GlobalMemory)
      }
      
      case m : AbstractMap => {
        val len = Type.getLength(m.ouT)

        // get the newly allocated mem objects
        val mems = allocate(m.f, memory).drop(memory.length)

        // multiply each of the newly allocated mem objects with the length for this map
        memory ++ mems.map( (mem) => {
          val size = mem.size
          OpenCLMemory(mem.variable, Expr.simplify(size * len), mem.t, GlobalMemory)
        })
      }

      case _ => memory
    }
    f.memory
  }
  
}
