package analysis

import analysis.AccessCounts.SubstitutionMap
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.generator.NDRange
import opencl.ir._
import opencl.ir.pattern._

import scala.collection.mutable

object AccessCounts {

  type SubstitutionMap = collection.immutable.Map[ArithExpr, ArithExpr]

  def apply(
    lambda: Lambda,
    localSize: NDRange = NDRange(?,?,?),
    globalSize: NDRange = NDRange(?,?,?),
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

  private val privateMemoriesForCounting = getReduceAndIteratePrivates

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

  def getLoads(memory: Memory, exact: Boolean = false): ArithExpr = {
    val numLoads = loads.filterKeys({
      case (mem, _, _) if mem == memory => true
      case _ => false
    }).values.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr)
    getExact(numLoads, exact)
  }

  def getStores(memory: Memory, exact: Boolean = false): ArithExpr = {
    val numStores = stores.filterKeys({
      case (mem, _, _) if mem == memory => true
      case _ => false
    }).values.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr)
    getExact(numStores, exact)
  }

  def accesses(memory: Memory, exact: Boolean = false): ArithExpr =
    getLoads(memory, exact) + getStores(memory, exact)

  def getLoads(addressSpace: OpenCLAddressSpace, exact: Boolean): ArithExpr =
    getExact(loadsToAddressSpaces(addressSpace), exact)

  def getStores(addressSpace: OpenCLAddressSpace, exact: Boolean): ArithExpr =
    getExact(storesToAddressSpaces(addressSpace), exact)

  def accesses(addressSpace: OpenCLAddressSpace, exact: Boolean): ArithExpr =
    getLoads(addressSpace, exact) + getStores(addressSpace, exact)

  def getLoads(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean): ArithExpr = {
    val key = (addressSpace, accessPattern)
    getExact(loadsToAddressSpacesWithPattern(key), exact)
  }

  def getStores(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean): ArithExpr = {
    val key = (addressSpace, accessPattern)
    getExact(storesToAddressSpacesWithPattern(key), exact)
  }

  def accesses(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean): ArithExpr =

    getLoads(addressSpace, accessPattern, exact) +
      getStores(addressSpace, accessPattern, exact)

  def scalarLoads(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false): ArithExpr = {

    val loads = loadsToAddressSpacesWithPatternAndWidth.
      foldLeft(Cst(0): ArithExpr)((acc, loadAndCount) => {
        val (load, count) = loadAndCount
        if (load._3 == Cst(1) && load._2 == accessPattern && load._1 == addressSpace)
          acc + count
        else
          acc
      })

    getExact(loads, exact)
  }

  def scalarStores(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false): ArithExpr = {

    val stores = storesToAddressSpacesWithPatternAndWidth.
      foldLeft(Cst(0): ArithExpr)((acc, storeAndCount) => {
        val (store, count) = storeAndCount
        if (store._3 == Cst(1) && store._2 == accessPattern && store._1 == addressSpace)
          acc + count
        else
          acc
      })

    getExact(stores, exact)
  }

  def vectorLoads(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false): ArithExpr = {

    val loads = loadsToAddressSpacesWithPatternAndWidth.
      foldLeft(Cst(0): ArithExpr)((acc, loadAndCount) => {
        val (load, count) = loadAndCount
        if (load._3 != Cst(1) && load._2 == accessPattern && load._1 == addressSpace)
          acc + count
        else
          acc
      })

    getExact(loads, exact)
  }

  def vectorStores(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false): ArithExpr = {

    val stores = storesToAddressSpacesWithPatternAndWidth.
      foldLeft(Cst(0): ArithExpr)((acc, storeAndCount) => {
        val (store, count) = storeAndCount
        if (store._3 != Cst(1) && store._2 == accessPattern && store._1 == addressSpace)
          acc + count
        else
          acc
      })

    getExact(stores, exact)
  }

  def vectorAccesses(addressSpace: OpenCLAddressSpace,
    accessPattern: AccessPattern, exact: Boolean = false): ArithExpr =
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

    if (patternMap.isDefinedAt(expr)) {

      val pattern = patternMap(expr)
      val memory = expr.mem
      val t = expr.t

      updateEntry(map, memory, pattern, t)
    }
  }

  private def updateEntry(
    map: mutable.Map[AccessKey, ArithExpr],
    memory: Memory,
    pattern: AccessPattern,
    t: Type): Unit =

    (pattern, t, memory) match {
      case (AccessPatternCollection(patterns), TupleType(tt@_*), OpenCLMemoryCollection(mems, _)) =>

        (patterns, tt, mems).
          zipped.
          filter((maybePattern, _, _) => maybePattern.isDefined).
          zipped.
          foreach((maybePattern, t, mem) => updateEntry(map, mem, maybePattern.get, t))

      case (_, _, m) =>

        val vectorWidth = Type.getValueType(t) match {
          case VectorType(_, n) => n
          case _ => Cst(1)
        }

        if (m.isInstanceOf[OpenCLMemory] &&
          m.asInstanceOf[OpenCLMemory].addressSpace == PrivateMemory &&
        !privateMemoriesForCounting.contains(m))
          return

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

          case Iterate(n, nestedLambda, _,  _, _) =>
            count(nestedLambda, n)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)
          case _: UserFun | _: VectorizeUserFun =>

            args.foreach(updateEntry(_, accessPatterns.getReadPatterns, loads))

            updateEntry(expr, accessPatterns.getWritePatterns, stores)

          case _ =>
        }

      case _ =>
    }

  }

}
