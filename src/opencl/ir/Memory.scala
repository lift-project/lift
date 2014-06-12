package opencl.ir

import ir._

abstract class OpenCLAddressSpace
object LocalMemory extends OpenCLAddressSpace
object GlobalMemory extends OpenCLAddressSpace

case class OpenCLMemory(variable: Var, size: Expr, addressSpace: OpenCLAddressSpace,
                        accessFunctions: Array[(Expr) => Expr] = Array.empty[(Expr) => Expr]) extends Memory

object OpenCLMemory {
  
  def allocate(f: Fun) : Unit = {
    val memory = allocate(f, Array.empty[Memory])
    println("TO ALLOCATE:")
    memory.foreach(println)
    println("DONE")
  }
  
  def allocate(f: Fun, memory: Array[Memory]): Array[Memory] = {

    f.memory = f match {
      
      case cf: CompFun => {
        cf.funs.foldRight(memory)((f, inM) => allocate(f, inM))        
      }
      
      // inputs (parameters) are always allocated in global memory in OpenCL
      case in : Input => {
        val size = Type.getSizeInBytes(in.ouT)
        memory :+ OpenCLMemory(in.variable, size, GlobalMemory)
      }
      
      // unsure about this ...
      // how is going to "do" the allocate?
      // RedSeq or Map (which one) ...
      case rf : ReduceSeq => {
        val size = Type.getSizeInBytes(rf.ouT)
        memory :+ OpenCLMemory(Var("NEW_RED"), size, GlobalMemory)
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
          OpenCLMemory(mem.variable, Expr.simplify(size * len), GlobalMemory)
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
