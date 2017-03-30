package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic.ArithExpr._
import lift.arithmetic._
import ir.Type
import ir.ast._
import ir.view._
import opencl.generator.OpenCLAST.VarRef
import opencl.generator.OpenCLGenerator.NDRange
import opencl.ir.pattern.{MapGlb, MapLcl}

import scala.collection.immutable


object AccessPatterns {

  def apply(lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new AccessPatterns(lambda, localSize, globalSize, valueMap)

}

abstract class AccessPattern
object CoalescedPattern extends AccessPattern
object UnknownPattern extends AccessPattern

class AccessPatterns(
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private var readPatterns = immutable.Map[Expr, AccessPattern]()
  private var writePatterns = immutable.Map[Expr, AccessPattern]()

  private var coalescingId: Option[Var] = None

  if (lambda.body.view == NoView)
    View(lambda)

  determinePatterns(lambda.body)

  def getReadPatterns: immutable.Map[Expr, AccessPattern] = readPatterns
  def getWritePatterns: immutable.Map[Expr, AccessPattern] = writePatterns

  def apply(): (immutable.Map[Expr, AccessPattern], immutable.Map[Expr, AccessPattern]) =
    (readPatterns, writePatterns)

  private def isCoalesced(v: VarRef, length: ArithExpr): Boolean = {
    val accessLocation = v.arrayIndex.content

    if (coalescingId.isEmpty)
      return false

    val newVar = Var()
    val i0 = substitute(accessLocation, immutable.Map(coalescingId.get -> (newVar + 0)))
    val i1 = substitute(accessLocation, immutable.Map(coalescingId.get -> (newVar + 1)))

    i1 - i0 == length
  }

  private def getAccessPattern(v: VarRef, length: ArithExpr) = {
    if (isCoalesced(v, length))
      Some(CoalescedPattern)
    else
      Some(UnknownPattern)
  }

  private def getAccessPattern(expr: Expr): Option[AccessPattern] = {
    val length = Type.getLength(Type.getValueType(expr.view.t))
    ViewPrinter.emit(Var(), expr.view) match {
      case v: VarRef => getAccessPattern(v, length)
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
              val accessPattern = getAccessPattern(arg)

              if (accessPattern.isDefined)
                readPatterns += arg -> accessPattern.get
            })

            writePatterns += expr -> getAccessPattern(expr).get

          case _ =>
        }

      case _ =>
    }

  }

}
