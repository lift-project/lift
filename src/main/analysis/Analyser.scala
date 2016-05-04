package analysis

import analysis.AccessCounts.SubstitutionMap
import apart.arithmetic.ArithExpr.{contains, substitute}
import apart.arithmetic._
import ir._
import ir.ast.Lambda
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator._
import opencl.ir._
import rewriting.InferNDRange.substituteInNDRange

class Analyser(
  val lambda: Lambda,
  val localSize: NDRange,
  val globalSize: NDRange,
  val valueMap: SubstitutionMap
) {

  if (lambda.body.t == UndefType)
    TypeChecker(lambda)

  if (lambda.body.mem == UnallocatedMemory) {
    RangesAndCounts(lambda, localSize, globalSize, valueMap)
    OpenCLMemoryAllocator(lambda)
  }

  protected val substLocal = substituteInNDRange(localSize, valueMap)
  protected val substGlobal = substituteInNDRange(globalSize, valueMap)

  protected val substitutionMap = collection.immutable.Map[ArithExpr, ArithExpr](
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

  protected def getExact(arithExpr: ArithExpr, exact: Boolean) =
    if (exact) substitute(arithExpr, substitutionMap) else arithExpr

}
