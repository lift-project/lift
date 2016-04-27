package analysis

import analysis.AccessCounts.SubstitutionMap
import apart.arithmetic.ArithExpr.{contains, substitute}
import apart.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator._
import opencl.ir._
import opencl.ir.pattern._
import rewriting.InferNDRange.substituteInNDRange

object AccessCounts {

  type SubstitutionMap = collection.immutable.Map[ArithExpr, ArithExpr]

  def apply(
    lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new AccessCounts(lambda, localSize, globalSize, valueMap)

}

class AccessCounts(
  val lambda: Lambda,
  val localSize: NDRange,
  val globalSize: NDRange,
  val valueMap: SubstitutionMap
) {

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

  private var loads = collection.Map[Memory, ArithExpr]()
  private var stores = collection.Map[Memory, ArithExpr]()

  private var currentNesting: ArithExpr = Cst(1)

  private lazy val loadsToAllAddressSpaces =
    loads
      .groupBy({ case (mem: OpenCLMemory, _) => mem.addressSpace })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))

  private lazy val storesToAllAddressSpaces =
    stores
      .groupBy({ case (mem: OpenCLMemory, _) => mem.addressSpace })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))

  if (lambda.body.t == UndefType)
    TypeChecker(lambda)

  if (lambda.body.mem == UnallocatedMemory) {
    RangesAndCounts(lambda, localSize, globalSize, valueMap)
    OpenCLMemoryAllocator(lambda)
  }

  count(lambda.body)

  private def getExact(arithExpr: ArithExpr, exact: Boolean) =
    if (exact) substitute(arithExpr, substitutionMap) else arithExpr

  override def toString: String = {
    val exact = true
    s"""Stores to global: ${getExact(storesToAddressSpace(GlobalMemory), exact)}
    |Loads from global: ${getExact(loadsToAddressSpace(GlobalMemory), exact)}
    |Stores to local: ${getExact(storesToAddressSpace(LocalMemory), exact)}
    |Loads from local: ${getExact(loadsToAddressSpace(LocalMemory), exact)}
    |Stores to private: ${getExact(storesToAddressSpace(PrivateMemory), exact)}
    |Loads form private: ${getExact(loadsToAddressSpace(PrivateMemory), exact)}""".stripMargin
  }

  def accesses = (loads, stores)

  // TODO: # accesses with pattern
  // TODO: # vector accesses

  def loadsToMemory(memory: Memory, exact: Boolean = false) =
    getExact(loads.getOrElse(memory, Cst(0)), exact)

  def storesToMemory(memory: Memory, exact: Boolean = false) =
    getExact(stores.getOrElse(memory, Cst(0)), exact)

  def accessesToMemory(memory: Memory, exact: Boolean = false) =
    loadsToMemory(memory, exact) + storesToMemory(memory, exact)

  def loadsToAddressSpace(addressSpace: OpenCLAddressSpace, exact: Boolean = false) =
    getExact(loadsToAllAddressSpaces.getOrElse(addressSpace, Cst(0)), exact)

  def storesToAddressSpace(addressSpace: OpenCLAddressSpace, exact: Boolean = false) =
    getExact(storesToAllAddressSpaces.getOrElse(addressSpace, Cst(0)), exact)

  def accessesToAddressSpace(addressSpace: OpenCLAddressSpace, exact: Boolean = false) =
    loadsToAddressSpace(addressSpace, exact) + storesToAddressSpace(addressSpace, exact)

  private def count(lambda: Lambda, arithExpr: ArithExpr): Unit = {
    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /^= arithExpr
  }

  private def count(expr: Expr): Unit = {

    expr match {
      case FunCall(f, args@_*) =>

        args.foreach(count)

        f match {
          case _: MapGlb | _: MapLcl | _:MapWrg =>
            val map = f.asInstanceOf[AbstractMap]

            val n = Type.getLength(expr.t) /^ map.loopVar.range.asInstanceOf[RangeAdd].step
            count(map.f, n)

          // TODO: Map?
          case mapSeq: MapSeq =>
            val n = Type.getLength(expr.t)
            count(mapSeq.f, n)

          // TODO: Reduce?
          case reduceSeq: ReduceSeq =>
            val n = Type.getLength(args(1).t)
            count(reduceSeq.f, n)

          case Iterate(n, nestedLambda) =>
            count(nestedLambda, n)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)
          case uf: UserFun =>

            args.foreach(expr => {
              val memory = expr.mem
              val loadsSoFar = loads.getOrElse(memory, Cst(0))
              loads += memory -> (loadsSoFar + currentNesting)
            })

            val memory = expr.mem
            val storesSoFar = stores.getOrElse(memory, Cst(0))
            stores += memory -> (storesSoFar + currentNesting)

          case _ =>
        }

      case _ =>
    }

  }

}
