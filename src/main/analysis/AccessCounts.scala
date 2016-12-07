package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLGenerator.NDRange
import opencl.ir._
import opencl.ir.pattern._

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
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private type AccessKey = (Memory, AccessPattern, ArithExpr)

  private val loads =
    collection.mutable.Map[AccessKey, ArithExpr]()
    .withDefaultValue(Cst(0))
  private val stores =
    collection.mutable.Map[AccessKey, ArithExpr]()
    .withDefaultValue(Cst(0))

  private var currentNesting: ArithExpr = Cst(1)

  private lazy val loadsToAddressSpacesWithPatternAndWidth =
    loads
      .groupBy({ case ((mem: OpenCLMemory, pattern, width), _) => (mem.addressSpace, pattern, width) })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))
      .withDefaultValue(Cst(0))

  private lazy val storesToAddressSpacesWithPatternAndWidth =
    stores
      .groupBy({ case ((mem: OpenCLMemory, pattern, width), _) => (mem.addressSpace, pattern, width) })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))
      .withDefaultValue(Cst(0))

  private lazy val loadsToAddressSpacesWithPattern =
    loads
      .groupBy({ case ((mem: OpenCLMemory, pattern, _), _) => (mem.addressSpace, pattern) })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))
      .withDefaultValue(Cst(0))

  private lazy val storesToAddressSpacesWithPattern =
    stores
      .groupBy({ case ((mem: OpenCLMemory, pattern, _), _) => (mem.addressSpace, pattern) })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))
      .withDefaultValue(Cst(0))

  private lazy val loadsToAddressSpaces =
    loads
      .groupBy({ case ((mem: OpenCLMemory, _, _), _) => mem.addressSpace })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))
      .withDefaultValue(Cst(0))

  private lazy val storesToAddressSpaces =
    stores
      .groupBy({ case ((mem: OpenCLMemory, _, _), _) => mem.addressSpace })
      .map(kv => (kv._1, kv._2.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)))
      .withDefaultValue(Cst(0))

  private val accessPatterns = AccessPatterns(lambda, localSize, globalSize, valueMap)

  count(lambda.body)

  override def toString: String = {
    val exact = true
    s"""Stores to global: ${getStores(GlobalMemory, exact)}
    |Loads from global: ${getLoads(GlobalMemory, exact)}
    |Stores to local: ${getStores(LocalMemory, exact)}
    |Loads from local: ${getLoads(LocalMemory, exact)}
    |Stores to private: ${getStores(PrivateMemory, exact)}
    |Loads form private: ${getLoads(PrivateMemory, exact)}""".stripMargin
  }

  def accesses = (loads, stores)

  def getLoads(memory: Memory, exact: Boolean = false) = {
    val numLoads = loads.filterKeys({
      case (mem, _, _) if mem == memory => true
      case _ => false
    }).values.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr)
    getExact(numLoads, exact)
  }

  def getStores(memory: Memory, exact: Boolean = false) = {
    val numStores = stores.filterKeys({
      case (mem, _, _) if mem == memory => true
      case _ => false
    }).values.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr)
    getExact(numStores, exact)
  }

  def accesses(memory: Memory, exact: Boolean = false) =
    getLoads(memory, exact) + getStores(memory, exact)

  def getLoads(addressSpace: OpenCLAddressSpace, exact: Boolean) =
    getExact(loadsToAddressSpaces(addressSpace), exact)

  def getStores(addressSpace: OpenCLAddressSpace, exact: Boolean) =
    getExact(storesToAddressSpaces(addressSpace), exact)

  def accesses(addressSpace: OpenCLAddressSpace, exact: Boolean) =
    getLoads(addressSpace, exact) + getStores(addressSpace, exact)

  def getLoads(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean) = {
    val key = (addressSpace, accessPattern)
    getExact(loadsToAddressSpacesWithPattern(key), exact)
  }

  def getStores(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean) = {
    val key = (addressSpace, accessPattern)
    getExact(storesToAddressSpacesWithPattern(key), exact)
  }

  def accesses(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean) =

    getLoads(addressSpace, accessPattern, exact) +
      getStores(addressSpace, accessPattern, exact)

  def vectorLoads(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false) = {

    loadsToAddressSpacesWithPatternAndWidth.
      foldLeft(Cst(0): ArithExpr)((acc, bla) =>
        if (bla._1._3 != Cst(1) && bla._1._2 == accessPattern && bla._1._1 == addressSpace)
          acc + bla._2
        else
          acc
      )
  }

  def vectorStores(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false) = {
    storesToAddressSpacesWithPatternAndWidth.
      foldLeft(Cst(0): ArithExpr)((acc, bla) =>
        if (bla._1._3 != Cst(1) && bla._1._2 == accessPattern && bla._1._1 == addressSpace)
          acc + bla._2
        else
          acc
      )
  }

  def vectorAccesses(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false) =
   vectorLoads(addressSpace, accessPattern, exact) +
     vectorStores(addressSpace, accessPattern, exact)

  private def count(lambda: Lambda, arithExpr: ArithExpr): Unit = {
    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /^= arithExpr
  }

  private def updateEntry(
    expr: Expr,
    patternMap: collection.Map[Expr, AccessPattern],
    map: collection.mutable.Map[AccessKey, ArithExpr]): Unit = {
    val memory = expr.mem

    val vectorWidth = Type.getValueType(expr.t) match {
      case VectorType(_, n) => n
      case _ => Cst(1)
    }

    val pattern = patternMap(expr)
    val key = (memory, pattern, vectorWidth)
    val loadsSoFar = map(key)
    map(key) = loadsSoFar + currentNesting
  }

  private def count(expr: Expr): Unit = {

    expr match {
      case FunCall(f, args@_*) =>

        args.foreach(count)

        f match {
          case _: MapGlb | _: MapLcl | _:MapWrg =>
            val map = f.asInstanceOf[AbstractMap]
            val step = map.loopVar.range.asInstanceOf[RangeAdd].step

            val n = Type.getLength(expr.t) /^ step
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

            args.foreach(updateEntry(_, accessPatterns.getReadPatterns, loads))

            updateEntry(expr, accessPatterns.getWritePatterns, stores)

          case _ =>
        }

      case _ =>
    }

  }

}
