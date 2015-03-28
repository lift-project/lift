package opencl.generator

import ir._
import opencl.ir._

object BarrierElimination {

  def apply(l: Lambda): Unit = {
    Expr.visit(l.body, (x: Expr) => {
      x match {
        case mapCall: MapCall =>
          println("Map, down a level")
        case call: FunCall =>
          markFunCall(call)
        case _ =>
      }
    }, (x: Expr) => {
      x match {
        case mapCall: MapCall =>
          println("up a level")
        case _ =>
      }
    })
  }

  def markFunCall(call: FunCall): Unit = {
    call.f match {
      case cf: CompFunDef =>
        val funs = cf.funs
        val c = funs.count(isConcrete)

        var next = funs
        var groups = Seq[Seq[Lambda]]()

        if (c > 0) {

          while (next.nonEmpty) {
            val prefixLength = next.prefixLength(!isConcrete(_))
            groups = groups :+ next.take(prefixLength + 1)
            next = next.drop(prefixLength + 1)
          }

          val barrierInHead = groups.head.exists(l => l.body.isInstanceOf[FunCall] &&
            l.body.asInstanceOf[FunCall].f.isInstanceOf[Barrier])

          // TODO: Or reads from local and is not in a loop
          if (barrierInHead && readsFrom(groups.head.last) == GlobalMemory) {
            invalidateBarrier(groups.head)
          }

          // TODO: reorder in global only needs one barrier
          // TODO: reorder in local needs one after being consumed if in a loop
          // TODO: can remove if several there is one anyway?
        }

      case _ =>
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
