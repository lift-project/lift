package opencl.generator

import ir._
import opencl.ir._

object BarrierElimination {

  def apply(l: Lambda): Unit = {
    apply(l.body, insideLoop = false)
  }

  def apply(expr: Expr, insideLoop: Boolean): Unit = {
    expr match {
      case call: MapCall =>
        apply(call.f.f.body, insideLoop || loop(call.iterationCount))
      case call: ReduceCall =>
        apply(call.f.f.body, insideLoop || loop(call.iterationCount))
      case call: IterateCall =>
        apply(call.f.f.body, insideLoop || loop(call.iterationCount))
      case call: FunCall => call.f match {
        case cf: CompFunDef =>
          markFunCall(cf, insideLoop)
          cf.funs.foreach( (l:Lambda) => apply(l.body, insideLoop) )
        case f: FPattern => apply(f.f.body, insideLoop)
        case l: Lambda => apply(l.body, insideLoop)
        case _: Zip => call.args.foreach(apply(_, insideLoop))
        case _ =>
      }
      case _ =>
    }
  }

  def loop(ae: ArithExpr): Boolean = {
    !(ae == Cst(1) || ae == Cst(0) || ae == Fraction(1, ?))
  }

  def markFunCall(cf: CompFunDef, insideLoop: Boolean): Unit = {
    val lambdas = cf.funs
    val c = lambdas.count(isConcrete)

    var next = lambdas
    var groups = Seq[Seq[Lambda]]()

    if (c > 0) {

      while (next.nonEmpty) {
        val prefixLength = next.prefixLength(!isConcrete(_))
        groups = groups :+ next.take(prefixLength + 1)
        next = next.drop(prefixLength + 1)
      }

      val barrierInHead = groups.head.exists(l => l.body.isInstanceOf[FunCall] &&
        l.body.asInstanceOf[FunCall].f.isInstanceOf[Barrier])

      val finalReadMemory = readsFrom(groups.head.last)
      println(finalReadMemory)
      if (barrierInHead && finalReadMemory == GlobalMemory ||
        barrierInHead && finalReadMemory == LocalMemory && !insideLoop) {
        invalidateBarrier(groups.head)
      }

      // TODO: reorder in global only needs one barrier
      // TODO: reorder in local needs one after being consumed if in a loop
      // TODO: can remove if several and there is one anyway?
    }
  }

  def invalidateBarrier(group: Seq[Lambda]): Unit = {
    group.find(l => l.body.isInstanceOf[FunCall] &&
      l.body.asInstanceOf[FunCall].f.isInstanceOf[Barrier]).
      get.body.asInstanceOf[FunCall].f.asInstanceOf[Barrier].valid = false
  }

  def isConcrete(l: Lambda): Boolean = {
    Expr.visit[Boolean](false)(l.body, (e, b) => {
      e match {
        case call: FunCall =>
          call.f match {
            case _: UserFunDef => true
            case _ => b
          }
        case _ => b
      }
    })
  }

  def hasBarrier(l: Lambda): Boolean = {
    Expr.visit[Boolean](false)(l.body, (e, b) => {
      e match {
        case call: FunCall =>
          call.f match {
            case _: Barrier => true
            case _ => b
          }
        case _ => b
      }
    })
  }

  def readsFrom(l:Lambda): OpenCLAddressSpace = {
    Expr.visit[OpenCLAddressSpace](UndefAddressSpace)(l.body, (e, b) => {
      e match {
        case call: FunCall =>
          call.f match {
            case uf: UserFunDef => if (b == UndefAddressSpace) OpenCLMemory.asOpenCLMemory(call.args(0).mem).addressSpace else b
            case _ => b
          }
        case _ => b
      }
    })
  }

}
