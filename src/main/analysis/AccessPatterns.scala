package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic.ArithExpr._
import lift.arithmetic._
import ir.{ScalarType, TupleType, Type, UndefType}
import ir.ast._
import ir.view._
import opencl.generator.{NDRange, OpenCLGenerator}
import core.generator.GenericAST.{ArithExpression, VarIdxRef}
import opencl.ir.OpenCLMemoryCollection
import opencl.ir.pattern.{MapGlb, MapLcl}

import scala.collection.immutable


object AccessPatterns {

  def apply(lambda: Lambda,
    localSize: NDRange = NDRange(?,?,?),
    globalSize: NDRange = NDRange(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new AccessPatterns(lambda, localSize, globalSize, valueMap)

}

abstract class AccessPattern

object CoalescedPattern extends AccessPattern
object UnknownPattern extends AccessPattern

case class AccessPatternCollection(comp: Seq[Option[AccessPattern]]) extends AccessPattern

class AccessPatterns(
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private var readPatterns = immutable.Map[Expr, AccessPattern]()
  private var writePatterns = immutable.Map[Expr, AccessPattern]()

  private val varDecls = OpenCLGenerator.getDifferentMemories(lambda)._3

  private var coalescingId: Option[Var] = None

  if (lambda.body.view == NoView)
    View(lambda)

  determinePatterns(lambda.body)

  def getReadPatterns: immutable.Map[Expr, AccessPattern] = readPatterns
  def getWritePatterns: immutable.Map[Expr, AccessPattern] = writePatterns

  def apply(): (immutable.Map[Expr, AccessPattern], immutable.Map[Expr, AccessPattern]) =
    (readPatterns, writePatterns)

  private def isCoalesced(v: VarIdxRef, length: ArithExpr): Boolean = {
    val accessLocation = v.arrayIndex.get.asInstanceOf[ArithExpression].content

    if (coalescingId.isEmpty)
      return false

    val newVar = Var()
    val i0 = substitute(accessLocation, immutable.Map(coalescingId.get -> (newVar + 0)))
    val i1 = substitute(accessLocation, immutable.Map(coalescingId.get -> (newVar + 1)))

    i1 - i0 == length
  }

  private def getAccessPattern(v: VarIdxRef, length: ArithExpr) = {
    if (isCoalesced(v, length))
      Some(CoalescedPattern)
    else
      Some(UnknownPattern)
  }

  private def getAccessPattern(view: View): Option[AccessPattern] = {
    val length = Type.getLength(Type.getValueType(view.t))
    ViewPrinter.emit(view) match {
      case v: VarIdxRef => getAccessPattern(v, length)
      case _ => None
    }
  }

  private def determinePatterns(expr: Expr): Unit = {

    expr match {
      case FunCall(f, args@_*) =>
        args.foreach(determinePatterns)

        f match {
          case mapLcl@MapLcl(0, nestedLambda) =>
            coalescingId = Some(mapLcl.loopVar)
            determinePatterns(nestedLambda.body)
            coalescingId = None

          case mapGlb@MapGlb(0, nestedLambda) =>
            coalescingId = Some(mapGlb.loopVar)
            determinePatterns(nestedLambda.body)
            coalescingId = None

          case lambda: Lambda => determinePatterns(lambda.body)
          case fp: FPattern => determinePatterns(fp.f.body)
          case _: UserFun | _: VectorizeUserFun =>

            args.foreach(arg => {

              val isMemoryCollection = arg.mem.isInstanceOf[OpenCLMemoryCollection]
              val isTupleType = arg.t.isInstanceOf[TupleType]
              val isScalarType = arg.t.isInstanceOf[ScalarType]

              val declaredType = varDecls.getOrElse(arg.mem.variable, UndefType)
              val declaredAsTupleType =
                Type.getValueType(declaredType).isInstanceOf[TupleType]

              if (isMemoryCollection && isTupleType) {

                val tt = arg.t.asInstanceOf[TupleType]
                val patterns = tt.elemsT.indices.map(i =>
                  getAccessPattern(arg.view.get(i)))

                readPatterns += arg -> AccessPatternCollection(patterns)

              } else if (isScalarType && declaredAsTupleType) {

                readPatterns += arg -> UnknownPattern

              } else {

                val accessPattern = getAccessPattern(arg.view)

                if (accessPattern.isDefined)
                  readPatterns += arg -> accessPattern.get
              }
            })

            writePatterns += expr -> getAccessPattern(expr.outputView).get

          case _ =>
        }

      case _ =>
    }

  }

}
