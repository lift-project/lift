package analysis

import analysis.AccessCounts.SubstitutionMap
import apart.arithmetic.ArithExpr.{contains, substitute}
import apart.arithmetic.{?, ArithExpr, Cst}
import ir.ast.{Expr, Lambda}
import ir.{Memory, NoType, TypeChecker, UnallocatedMemory}
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator._
import opencl.ir.{GlobalMemory, LocalMemory, OpenCLMemoryAllocator, TypedOpenCLMemory}
import rewriting.InferNDRange.substituteInNDRange

object MemoryAmounts {
    def apply(
    lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new MemoryAmounts(lambda, localSize, globalSize, valueMap)

}

class MemoryAmounts(
  val lambda: Lambda,
  val localSize: NDRange,
  val globalSize: NDRange,
  val valueMap: SubstitutionMap) {

  private val substLocal = substituteInNDRange(localSize, valueMap)
  private val substGlobal = substituteInNDRange(globalSize, valueMap)

  private val substitutionMap = collection.immutable.Map[ArithExpr, ArithExpr](
    new get_local_size(0) -> substLocal(0),
    new get_local_size(1) -> substLocal(1),
    new get_local_size(2) -> substLocal(2),
    new get_global_size(0) -> substGlobal(0),
    new get_global_size(1) -> substGlobal(1),
    new get_global_size(2) -> substGlobal(2),
    new get_num_groups(0) -> (substGlobal(0) / substLocal(0)),
    new get_num_groups(1) -> (substGlobal(1) / substLocal(1)),
    new get_num_groups(2) -> (substGlobal(2) / substLocal(2))
  ).filterNot(pair => contains(pair._2, ?)) ++ valueMap

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

  private def getExact(arithExpr: ArithExpr, exact: Boolean) =
    if (exact) substitute(arithExpr, substitutionMap) else arithExpr

  def getGlobalMemoryUsed(exact: Boolean = false) = getExact(globalMemoryUsed, exact)
  def getLocalMemoryUsed(exact: Boolean = false) = getExact(localMemoryUsed, exact)
  def getPrivateMemoryUsed(exact: Boolean = false) = getExact(privateMemoryUsed, exact)

  determine()

  private def determine(): Unit = {

    if (lambda.body.t == NoType)
      TypeChecker(lambda)

    if (lambda.body.mem == UnallocatedMemory) {
      // Allocate memory
      RangesAndCounts(lambda, localSize, globalSize, valueMap)
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
