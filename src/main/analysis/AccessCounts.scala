package analysis

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.executor.Compile
import opencl.ir.{OpenCLAddressSpace, OpenCLMemory}
import opencl.ir.pattern._

object AccessCounts {

  def apply(lambda: Lambda) =
    new AccessCounts(lambda)

}

class AccessCounts(val lambda: Lambda) {

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

  // TODO: only do the required steps and only if not already done
  Compile(lambda)
  count(lambda.body)

  def accesses = (loads, stores)

  // TODO: # accesses with pattern
  // TODO: # vector accesses

  def loadsToMemory(memory: Memory) = loads.getOrElse(memory, Cst(0))

  def storesToMemory(memory: Memory) = stores.getOrElse(memory, Cst(0))

  def accessesToMemory(memory: Memory) =
    loadsToMemory(memory) + storesToMemory(memory)


  def loadsToAddressSpace(addressSpace: OpenCLAddressSpace) =
    loadsToAllAddressSpaces.getOrElse(addressSpace, Cst(0))

  def storesToAddressSpace(addressSpace: OpenCLAddressSpace) =
    storesToAllAddressSpaces.getOrElse(addressSpace, Cst(0))

  def accessesToAddressSpace(addressSpace: OpenCLAddressSpace) =
    loadsToAddressSpace(addressSpace) + storesToAddressSpace(addressSpace)

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

            val n = Type.getLength(expr.t) / map.loopVar.range.asInstanceOf[RangeAdd].step
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
