package analysis

import analysis.AccessCounts.SubstitutionMap
import apart.arithmetic.{?, Cst}
import ir.ast.{Expr, Lambda}
import ir.{Memory, UnallocatedMemory}
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator._
import opencl.ir.{GlobalMemory, InferOpenCLAddressSpace, LocalMemory, OpenCLMemoryAllocator, TypedOpenCLMemory}

object MemoryAmounts {
    def apply(
    lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
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

    // Get the allocated buffers
    val kernelMemory = TypedOpenCLMemory.get(lambda.body, lambda.params)
    val buffers = TypedOpenCLMemory.get(lambda.body, lambda.params, includePrivate = true)

    valueMemories =
      Expr.visitWithState(Set[Memory]())(lambda.body, (lambda, set) =>
        lambda match {
          case value: ir.ast.Value => set + value.mem
          case _ => set
        })

    privateMemories =
      buffers.diff(kernelMemory).partition(m => valueMemories.contains(m.mem))._2

    localMemories = buffers.filter(_.mem.addressSpace == LocalMemory)
    globalMemories = buffers.filter(_.mem.addressSpace == GlobalMemory)
  }

}
