package analysis

import analysis.AccessCounts.SubstitutionMap
import ir.ast.{AbstractPartRed, Expr, FunCall, Iterate, Lambda}
import ir.{Memory, UnallocatedMemory}
import lift.arithmetic.{?, Cst}
import opencl.generator._
import opencl.ir.{CollectTypedOpenCLMemory, GlobalMemory, InferOpenCLAddressSpace, LocalMemory, OpenCLMemory, OpenCLMemoryAllocator, PrivateMemory, TypedOpenCLMemory}

object MemoryAmounts {
    def apply(
    lambda: Lambda,
    localSize: NDRange = NDRange(?,?,?),
    globalSize: NDRange = NDRange(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new MemoryAmounts(lambda, localSize, globalSize, valueMap)

}

class MemoryAmounts(
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private var globalMemories = Seq[TypedOpenCLMemory]()
  private var localMemories = Seq[TypedOpenCLMemory]()
  private var privateMemories = Seq[TypedOpenCLMemory]()
  private var valueMemories = Set[Memory]()

  private lazy val globalMemoryUsed =
    globalMemories.map(_.mem.size).fold(Cst(0))(_ + _)

  private lazy val localMemoryUsed =
    localMemories.map(_.mem.size).fold(Cst(0))(_ + _)

  private lazy val privateMemoryUsed =
    privateMemories.map(_.mem.size).fold(Cst(0))(_ + _)

  def getGlobalMemories = globalMemories
  def getLocalMemories = localMemories
  def getPrivateMemories = privateMemories

  def getGlobalMemoryUsed(exact: Boolean = false) = getExact(globalMemoryUsed, exact)
  def getLocalMemoryUsed(exact: Boolean = false) = getExact(localMemoryUsed, exact)
  def getPrivateMemoryUsed(exact: Boolean = false) = getExact(privateMemoryUsed, exact)

  determine()

  private def determine(): Unit = {

    if (lambda.body.mem == UnallocatedMemory) {
      // Allocate memory
      RangesAndCounts(lambda, localSize, globalSize, valueMap)
      InferOpenCLAddressSpace(lambda)
      OpenCLMemoryAllocator(lambda)
    }

    val allowedPrivate = getReduceAndIteratePrivates

    // Get the allocated buffers
    val kernelMemory = CollectTypedOpenCLMemory.asFlatSequence(lambda)
    val buffers = CollectTypedOpenCLMemory.asFlatSequence(lambda, includePrivate = true)

    valueMemories =
      Expr.visitWithState(Set[Memory]())(lambda.body, (lambda, set) =>
        lambda match {
          case value: ir.ast.Value => set + value.mem
          case _ => set
        })

    privateMemories =
      buffers.
        diff(kernelMemory).
        filterNot(m => valueMemories.contains(m.mem)).
        filter(m => allowedPrivate.contains(m.mem))

    localMemories = buffers.filter(_.mem.addressSpace == LocalMemory)
    globalMemories = buffers.filter(_.mem.addressSpace == GlobalMemory)
  }

}
