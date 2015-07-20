package opencl.generator

import arithmetic.{?, ArithExpr, Cst, IntDiv}
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

/**
 * A pass for eliminating unnecessary barriers.
 *
 * Barriers are only necessary when the same memory locations get reused (they are in
 * a loop) or if different threads try to read memory locations other threads write to.
 *
 * The pass visits all sub-expressions and determines where splits, joins and reorders
 * take place (they cause threads to interact) and whether the sub-expressions appear
 * inside loops (causes reuse of memory locations).
 *
 */
object BarrierElimination {

  /**
   * Visit the lambda and determine which barriers can be eliminated.
   *
   * @param lambda The starting lambda.
   */
  def apply(lambda: Lambda): Unit = {
    apply(lambda.body, insideLoop = false)
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
        case cf: CompFun =>
          markFunCall(cf, insideLoop)
          cf.funs.foreach( (l:Lambda) => apply(l.body, insideLoop) )
        case f: FPattern => apply(f.f.body, insideLoop)
        case l: Lambda => apply(l.body, insideLoop)
        case Zip(_) | Tuple(_) =>
          val numAddressSpaces =
            call.args.map(m => OpenCLMemory.asOpenCLMemory(m.mem).addressSpace).
              distinct.length != 1

          // Only the last argument needs a barrier if all of them are in
          // the same address space
          call.args.foldRight(insideLoop)((e, loop) => {
            apply(e, loop)
            numAddressSpaces
          })
        case _ =>
      }
      case _ =>
    }
  }

  private def isLoop(ae: ArithExpr): Boolean = {
    !(ae == Cst(1) || ae == Cst(0) || ae == IntDiv(1, ?))
  }

  private def getLambdas(e: Expr): List[Lambda] = {
    e match {
      case call: FunCall =>
        val argLambdas = call.args.foldLeft(List[Lambda]())((ll, f) => ll ++ getLambdas(f))
        call.f match {
          case cf: CompFun => cf.funs.toList ++ argLambdas
          case _ => argLambdas
        }
      case _ => List()
    }
  }

  private def flatten(compFunDef: CompFun) : List[Lambda] = {
    compFunDef.funs.foldLeft(List[Lambda]())((ll, f) => {
      f.body match {

        case call: FunCall =>
          val argLambdas = call.args.foldLeft(List[Lambda]())((ll, f) => ll ++ getLambdas(f))

          call.f match {
            case cf: CompFun => ll ++ flatten(cf) ++ argLambdas
            case _ => ll :+ f
          }
        case _ => ll :+ f
      }
    })
  }

  private def markFunCall(cf: CompFun, insideLoop: Boolean): Unit = {
    // Flatten the CompFunDef, such that computations/reorders in
    // zip/tuple operations appear as well
    val lambdas = flatten(cf)
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
      val finalReadMemory = readsFrom(groups.head.last)
      needsBarrier(0) =
        if (groups.length > 1)
          !(barrierInHead && (finalReadMemory == GlobalMemory || finalReadMemory == LocalMemory && !insideLoop))
        else
          (OpenCLMemory.containsLocalMemory(groups.head.last.body.mem) ||
            groups.head.last.params.foldLeft(false)((needsBarrier, param) => {
              OpenCLMemory.containsLocalMemory(param.mem) || needsBarrier
            }) ) && insideLoop

      groups.zipWithIndex.foreach(x => {
        val group = x._1
        val id = x._2

        // Conservative assumption. TODO: Not if only has matching splits and joins
        if (group.exists(l => isSplit(l) || isJoin(l)) && id > 0) {
          needsBarrier(id) = true

          // Split/Join in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (OpenCLMemory.containsLocalMemory(group.last.body.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(isBarrier)).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Scatter affects the writing of this group and therefore the reading of the
        // group before. Gather in init affects the reading of the group before
        if (group.exists(isScatter) || group.init.exists(isGather) || group.exists(isTranspose)) {
          needsBarrier(id) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (OpenCLMemory.containsLocalMemory(group.last.body.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(isBarrier)).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Gather in last affects the reading of this group
        if (isGather(group.last) && id < groups.length - 1) {
          needsBarrier(id + 1) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (readsFrom(group.last) == LocalMemory && id > 0 && insideLoop &&
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
    l.body.isInstanceOf[FunCall] && l.body.asInstanceOf[FunCall].f.isInstanceOf[Gather]
  }

  private def isScatter(l: Lambda): Boolean = {
    l.body.isInstanceOf[FunCall] && l.body.asInstanceOf[FunCall].f.isInstanceOf[Scatter]
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

  private def readsFrom(lambda: Lambda): OpenCLAddressSpace = {
    Expr.visitWithState[OpenCLAddressSpace](UndefAddressSpace)(lambda.body, (expr, addressSpace) => {
      expr match {
        case call: FunCall =>
          call.f match {
            case uf: UserFun =>
              if (addressSpace == UndefAddressSpace)
                OpenCLMemory.asOpenCLMemory(call.args(0).mem).addressSpace
              else
                addressSpace
            case _ => addressSpace
          }
        case _ => addressSpace
      }
    })
  }
}
