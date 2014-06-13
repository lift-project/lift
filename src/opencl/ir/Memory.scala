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
    // TODO: get rid of the stupid cast!
    //val currentAddressSpace = memory.last.asInstanceOf[OpenCLMemory].addressSpace

    f.memory = f match {
      
      case cf: CompFun => {
        cf.funs.foldRight(memory)((f, mem) => allocate(f, mem))        
      }
      
      // inputs (parameters) are always allocated in global memory in OpenCL
      case in : Input => {
        val size = Type.getSizeInBytes(in.ouT)
        memory :+ OpenCLMemory(in.variable, size, in.ouT, GlobalMemory)
      }
      
      // the sequential implementations add
      case rf : ReduceSeq => {
        val size = Type.getSizeInBytes(rf.ouT)
        memory :+ OpenCLMemory(Var(), size, rf.ouT, GlobalMemory)
      }
      
      case m : AbstractMap => {
        /*
        // prepare the access fun
        val elemT = Type.getElemT(m.inT)
    
	    // multiply all lengths with the indexVariable ...
        // indexVar == g_id or l_id How to deal with that ????
        val expr = Type.length(elemT).foldLeft(indexVar)( _ * _ )
        val accessFun = (index: Expr) => { expr + index }
        */
        
        val mems = allocate(m.f, memory)
        val len = Type.getLength(m.ouT)
        mems.map( (mem) => {
          val size = mem.size
          OpenCLMemory(mem.variable, Expr.simplify(size * len), mem.t, GlobalMemory)
        })
      }
      
      /*
      case AbstractReduce(inF) => {
        val elemT = getElemT(inT)
        check(inF, TupleType(elemT, elemT)) // TODO change this probably
        ArrayType(elemT, new Cst(1))
      }
      
      case _:asScalar  => inT match {     
        case at: ArrayType => asScalar(at)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }          
                          
      case asVector(len) => inT match {
        case at: ArrayType => asVector(at, len)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      */

      case _ => memory
    }
    f.memory
  }
  
}
