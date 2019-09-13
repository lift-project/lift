package backends.c.common.common_ir

import ir.Memory
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.ir.{AddressSpaceCollection, OpenCLAddressSpace, UndefAddressSpace}


//Notice that we reuse OpenCLAddressSpace instead of invent new address space
//Because we want to reuse the view system infrastructure

class HostMemory(val variable: Var, val size: ArithExpr, val openCLAddressSpace: OpenCLAddressSpace) extends Memory

object HostMemory {
  def apply(variable: Var, size: ArithExpr, openCLAddressSpace: OpenCLAddressSpace): HostMemory
  = new HostMemory(variable, size, openCLAddressSpace)
}

case class HostMemoryCollection(subMemories: Array[HostMemory], override val openCLAddressSpace: AddressSpaceCollection)
  extends HostMemory(Var("Tuple"), subMemories.distinct.map(_.size).reduce(_+_), CPUMainMemoryAddressSpace  )

object HostMemoryCollection {
  def apply(mems: Seq[HostMemory]): HostMemoryCollection = {
    val addressSpace = AddressSpaceCollection(mems.map(m => m.openCLAddressSpace))
    new HostMemoryCollection(mems.toArray, addressSpace)
  }
}


object CPUNullMemory
  extends HostMemory(Var("NULL"), Cst(-1), UndefAddressSpace)

