package analysis

import apart.arithmetic._
import apart.arithmetic.ArithExpr._
import ir.view._
import ir.ast.{Map => _, _}
import opencl.executor.Compile
import opencl.ir.pattern.{MapGlb, MapLcl}


object AccessPatterns {

  def apply(lambda: Lambda) =
    new AccessPatterns(lambda)

}

abstract class AccessPattern
object CoalescedPattern extends AccessPattern
object UnknownPattern extends AccessPattern

class AccessPatterns(val lambda: Lambda) {

  private var readPatterns = Map[Expr, AccessPattern]()
  private var writePatterns = Map[Expr, AccessPattern]()

  private var coalescingId: Option[Var] = None

  // TODO: only do the required steps and only if not already done
  Compile(lambda)
  determinePatterns(lambda.body)

  def apply() =
    (readPatterns, writePatterns)

  private def isCoalesced(view: View) = {
    val newVar = Var("")
    val accessLocation = ViewPrinter.emit(view)

    val i0 = substitute(accessLocation, Map(coalescingId.get -> (newVar + 0)))
    val i1 = substitute(accessLocation, Map(coalescingId.get -> (newVar + 1)))

    i1 - i0 == Cst(1)
  }

  private def getPattern(view: View) = {
    if (isCoalesced(view))
      CoalescedPattern
    else
      UnknownPattern
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

          case fp: FPattern => determinePatterns(fp.f.body)
          case uf: UserFun =>

            args.foreach(arg =>
              readPatterns += arg -> getPattern(arg.view)
            )

            writePatterns += expr -> getPattern(expr.outputView)

          case _ =>
        }

      case _ =>
    }

  }

}
