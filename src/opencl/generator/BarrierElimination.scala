package opencl.generator

import arithmetic.{Fraction, Cst, ?, ArithExpr}
import ir._
import opencl.ir._

object BarrierElimination {

  def apply(l: Lambda): Unit = {
    apply(l.body, insideLoop = false)
  }

  private def apply(expr: Expr, insideLoop: Boolean): Unit = {
    expr match {
      case call: MapCall =>
        apply(call.f.f.body, insideLoop || isLoop(call.iterationCount))
      case call: ReduceCall =>
        apply(call.f.f.body, insideLoop || isLoop(call.iterationCount))
      case call: IterateCall =>
        apply(call.f.f.body, insideLoop || isLoop(call.iterationCount))
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

  private def isLoop(ae: ArithExpr): Boolean = {
    !(ae == Cst(1) || ae == Cst(0) || ae == Fraction(1, ?))
  }

  private def markFunCall(cf: CompFunDef, insideLoop: Boolean): Unit = {
    val lambdas = cf.funs
    val c = lambdas.count(_.body.isConcrete)

    var next = lambdas
    var groups = Seq[Seq[Lambda]]()

    if (c > 0) {

      // Partition the functions into groups such that the last element
      // of a group is concrete, except possibly in the last group
      while (next.nonEmpty) {
        val prefixLength = next.prefixLength(_.body.isAbstract)
        groups = groups :+ next.take(prefixLength + 1)
        next = next.drop(prefixLength + 1)
      }

      // If it is not concrete, then there can't be a barrier
      if (groups.last.last.body.isAbstract)
        groups = groups.init

      val needsBarrier = Array.fill(groups.length)(false)

      val barrierInHead = groups.head.exists(isBarrier)
      val finalReadMemory = groups.head.last.body.readsFrom
      needsBarrier(0) = !(barrierInHead && (finalReadMemory == GlobalMemory ||
        finalReadMemory == LocalMemory && !insideLoop))

      groups.zipWithIndex.foreach(x => {
        val group = x._1
        val id = x._2

        // Conservative assumption. TODO: Not if only has matching splits and joins
        if (group.exists(l => isSplit(l) || isJoin(l)) && id > 0) {
          needsBarrier(id) = true

          // Split/Join in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (group.last.body.writesTo == LocalMemory && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(isBarrier)).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Scatter affects the writing of this group and therefore the reading of the
        // group before. Gather in init affects the reading of the group before
        if (group.exists(isScatter) || group.init.exists(isGather) || group.exists(isTranspose)) {
          needsBarrier(id) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (group.last.body.writesTo == LocalMemory && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(isBarrier)).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Gather in last affects the reading of this group
        if (isGather(group.last) && id < groups.length - 1) {
          needsBarrier(id + 1) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (group.last.body.readsFrom == LocalMemory && id > 0 && insideLoop &&
            !groups.slice(0, id).map(_.exists(isBarrier)).reduce(_ || _))
            needsBarrier(id) = true
        }
      })

      (groups, needsBarrier).zipped.foreach((group, valid) => if (!valid) invalidateBarrier(group))
    }
  }

  private def isBarrier(l: Lambda): Boolean = {
    l.body.isInstanceOf[FunCall] && l.body.asInstanceOf[FunCall].f.isInstanceOf[Barrier]
  }

  private def isGather(l: Lambda): Boolean = {
    l.body match {
      case call: FunCall =>
        call match {
          case _: MapCall => false
          case _: ReduceCall => false
          case _: IterateCall => false
          case _ =>
            call.f match {
              case _: Gather => true
              case fp: FPattern => isGather(fp.f)
              case _ => false
            }
        }
      case _ => false
    }
  }

  private def isScatter(l: Lambda): Boolean = {
    l.body match {
      case call: FunCall =>
        call match {
          case _: MapCall => false
          case _: ReduceCall => false
          case _: IterateCall => false
          case _ =>
            call.f match {
              case _: Scatter => true
              case fp: FPattern => isScatter(fp.f)
              case _ => false
            }
        }
      case _ => false
    }
  }

  private def isSplit(l: Lambda): Boolean = {
    l.body.isInstanceOf[FunCall] && l.body.asInstanceOf[FunCall].f.isInstanceOf[Split]
  }

  private def isTranspose(l: Lambda): Boolean = {
    l.body.isInstanceOf[FunCall] &&
      (l.body.asInstanceOf[FunCall].f.isInstanceOf[Transpose] || l.body.asInstanceOf[FunCall].f.isInstanceOf[TransposeW])
  }

  private def isJoin(l: Lambda): Boolean = {
    l.body.isInstanceOf[FunCall] && l.body.asInstanceOf[FunCall].f.isInstanceOf[Join]
  }

  private def invalidateBarrier(group: Seq[Lambda]): Unit = {
    group.find(isBarrier) match {
      case Some(b) => b.body.asInstanceOf[FunCall].f.asInstanceOf[Barrier].valid = false
      case None =>
    }
  }
}
