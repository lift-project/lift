package analysis

import analysis.AccessCounts.SubstitutionMap
import apart.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator._
import opencl.ir.pattern._
import opencl.ir.{OpenCLMemory, OpenCLMemoryCollection, PrivateMemory, TypedOpenCLMemory}

object ControlFlow {
 def apply(
    lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new ControlFlow(lambda, localSize, globalSize, valueMap)
}

class ControlFlow(
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private var ifStatements: ArithExpr = Cst(0)
  private var forStatements: ArithExpr = Cst(0)
  private var currentNesting: ArithExpr = Cst(1)

  // TODO: Duplication with OpenCLGenerator
  type SymbolTable = collection.immutable.Map[Var, Type]
  private var varDecls: SymbolTable = collection.immutable.Map.empty

  // TODO: Duplication with OpenCLGenerator
  private val memory = TypedOpenCLMemory.get(lambda.body, lambda.params)

  private val valMems = Expr.visitWithState(Set[Memory]())(lambda.body, (expr, set) =>
    expr match {
      case value: Value => set + value.mem
      case _ => set
    })

  private val typedMems =
    TypedOpenCLMemory.get(lambda.body, lambda.params, includePrivate = true).toArray


  private val (typedValueMems, privateMems) =
    typedMems.diff(memory).partition(m => valMems.contains(m.mem))


  // the base type is used for allocation of all variables ...
  varDecls =
    typedMems.map(tm => {
      if (tm.mem.addressSpace == PrivateMemory) {
        // do not devectorize for private memory
        (tm.mem.variable, tm.t)
      } else {
        (tm.mem.variable, Type.devectorize(tm.t))
      }
    }).toMap

  // ... besides the these variables which use the value types
  // (i.e., possibly a vector type)
  varDecls = varDecls ++
    typedValueMems.map(tm => (tm.mem.variable, tm.t)).toMap

  count(lambda.body)

  def getIfStatements(exact: Boolean = false) =
    getExact(ifStatements, exact)

  def getForStatements(exact: Boolean = false) =
    getExact(forStatements, exact)

  // TODO: Duplication with OpenCLGenerator
  private def getOriginalType(mem: OpenCLMemory): Type = {

    try {
      varDecls(mem.variable)
    } catch {
      case _: NoSuchElementException =>
        throw new VariableNotDeclaredError(s"Trying to generate load to variable " +
          s"${mem.variable} which was not previously " +
          s"declared.")
    }
  }

  // TODO: Duplication with OpenCLGenerator
  private def existsInPrivateMemories(mem: Memory): Boolean =
    privateMems.exists(_.mem == mem)

  // TODO: Duplication with OpenCLGenerator
  private def shouldUnrollLoop(call: FunCall): Boolean = {
    var originalType: Type = UndefType
    try {
      originalType = getOriginalType(call.args.head.mem.asInstanceOf[OpenCLMemory])
    } catch {
      case _: VariableNotDeclaredError =>
    }
    val currentType = call.args.head.t

    val loopingOverVectorComponents = (originalType, currentType) match {
      case (_: VectorType, ArrayType(_: ScalarType, _)) => true
      case _ => false
    }

    loopingOverVectorComponents ||
      (OpenCLMemory.containsPrivateMemory(call.args.head.mem)
        && (call.args.head.mem match {
        case coll: OpenCLMemoryCollection =>
          coll.subMemories.exists(mem => existsInPrivateMemories(mem))
        case _ => existsInPrivateMemories(call.args.head.mem)
      })) ||
      // Don't unroll just for value
      OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory
  }

  private def count(
    lambda: Lambda,
    loopVar: Var,
    arithExpr: ArithExpr,
    unrolled: Boolean): Unit = {

    val range = loopVar.range.asInstanceOf[RangeAdd]
    // TODO: Information needed elsewhere. See OpenCLGenerator
    // try to see if we really need a loop
    loopVar.range.numVals match {
      case Cst(0) => return
      case Cst(1) =>

      // TODO: See TestOclFunction.numValues and issue #62
      case numVals if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
        ifStatements += currentNesting
      case _ =>
        (loopVar.range.numVals.min, loopVar.range.numVals.max) match {
          case (Cst(0),Cst(1)) =>
            // one or less iteration
            ifStatements += currentNesting

          case _  if !unrolled =>
            forStatements += currentNesting
          case _ =>
        }
    }

    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /^= arithExpr
  }

  private def count(expr: Expr): Unit = {
    expr match {
      case call@FunCall(f, args@_*) =>

        args.foreach(count)

        f match {
          case _: MapGlb | _: MapLcl | _: MapWrg =>
            val map = f.asInstanceOf[AbstractMap]
            val step = map.loopVar.range.asInstanceOf[RangeAdd].step

            val n = Type.getLength(expr.t) /^ step
            val unrolled = map.isInstanceOf[MapLcl] && shouldUnrollLoop(call)

            count(map.f, map.loopVar, n, unrolled)

          case mapSeq: MapSeq =>
            val n = Type.getLength(expr.t)
            count(mapSeq.f, mapSeq.loopVar, n, shouldUnrollLoop(call))

          case reduceSeq: ReduceSeq =>
            val n = Type.getLength(args(1).t)

            // TODO: Separate pass. Duplication with OpenCLGenerator
            val unroll = OpenCLMemory.containsPrivateMemory(args(1).mem)

            count(reduceSeq.f, reduceSeq.loopVar, n, unroll)

          case iterate@Iterate(n, nestedLambda) =>
            count(nestedLambda, iterate.indexVar, n, unrolled = false)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)

          case _ =>
        }
      case _ =>
    }
  }

}
