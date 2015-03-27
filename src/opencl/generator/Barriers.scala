package opencl.generator

import ir._
import opencl.ir._

object Barriers {

  def mark(l: Lambda): Unit = {
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

  def mark(e: Expr): Unit = {
    e match {
      case call: FunCall => markFunCall(call)
      case _ =>
    }
  }

  def markFunCall(call: FunCall): Unit = {
    call.f match {
      case cf: CompFunDef =>
        val funs = cf.funs
        val c = funs.count(isConcrete)

        var next = funs
        var groups = Seq(Seq[Lambda]())

        if (c > 0) {

          while (next.nonEmpty) {
            val prefixLength = funs.prefixLength(!isConcrete(_))
            groups = groups :+ funs.take(prefixLength + 1)
            next = funs.drop(prefixLength + 1)
          }

          groups.head.exists(_.f)
        }

      case _ =>
    }
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
