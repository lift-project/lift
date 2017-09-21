package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic.ArithExpr.{contains, substitute}
import lift.arithmetic._
import ir._
import ir.ast.{AbstractMap, AbstractPartRed, Expr, FunCall, Iterate, Lambda}
import opencl.generator._
import opencl.ir._
import rewriting.InferNDRange.substituteInNDRange

class Analyser(
  val lambda: Lambda,
  val localSize: NDRange,
  val globalSize: NDRange,
  val valueMap: SubstitutionMap
) {

  protected val substLocal = substituteInNDRange(localSize, valueMap)
  protected val substGlobal = substituteInNDRange(globalSize, valueMap)

  protected val substitutionMap = collection.immutable.Map[ArithExpr, ArithExpr](
    get_local_size(0) -> substLocal(0),
    get_local_size(1) -> substLocal(1),
    get_local_size(2) -> substLocal(2),
    get_global_size(0) -> substGlobal(0),
    get_global_size(1) -> substGlobal(1),
    get_global_size(2) -> substGlobal(2),
    get_num_groups(0) -> (substGlobal(0) / substLocal(0)),
    get_num_groups(1) -> (substGlobal(1) / substLocal(1)),
    get_num_groups(2) -> (substGlobal(2) / substLocal(2))
  ).filterNot(pair => contains(pair._2, ?)) ++ valueMap

  protected def getExact(arithExpr: ArithExpr, exact: Boolean) =
    if (exact) substitute(arithExpr, substitutionMap) else arithExpr

  protected def getParallelMapTripCount(map: AbstractMap, t: Type) = {
    Type.getLength(t) /^ map.loopVar.range.asInstanceOf[RangeAdd].step
  }

  if (lambda.body.t == UndefType)
    TypeChecker(lambda)

  if (lambda.body.mem == UnallocatedMemory) {
    RangesAndCounts(lambda, localSize, globalSize, valueMap)
    InferOpenCLAddressSpace(lambda)
    OpenCLMemoryAllocator(lambda)
  }

  protected def getReduceAndIteratePrivates: Seq[OpenCLMemory] = {
    var allowedPrivate = Seq[OpenCLMemory]()

    Expr.visit(lambda.body, _ => Unit, {
      case FunCall(_: AbstractPartRed, init, _) =>
        if (init.mem.asInstanceOf[OpenCLMemory].addressSpace == PrivateMemory)
          allowedPrivate = allowedPrivate :+ init.mem.asInstanceOf[OpenCLMemory]

        // TODO: OpenCLMemoryCollection/Tuple
        if (init.mem.isInstanceOf[OpenCLMemoryCollection])
          throw new NotImplementedError

      case FunCall(_: Iterate, _) =>
        throw new NotImplementedError
      case _ =>
    })

    allowedPrivate
  }

}
