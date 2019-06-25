package opencl.generator

import lift.arithmetic.{?, ArithExpr, Cst, IntDiv, Range, RangeAdd}
import rewriting.utils._
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
    new BarrierElimination(lambda).apply(lambda.body, insideLoop = false)
  }

  private[generator] def isPattern[T](funCall: FunCall, patternClass: Class[T]): Boolean = {
    Expr.visitWithState(false)(funCall, (expr, contains) => {
      expr match {
        case FunCall(declaration, _*) => declaration.getClass == patternClass || contains
        case _ => contains
      }
    }, visitArgs = false)
  }
}

class BarrierElimination(lambda: Lambda) {

  def apply(lambda: Lambda): Unit = {
    apply(lambda.body, insideLoop = false)
  }

  private def apply(expr: Expr, insideLoop: Boolean): Unit = {
    expr match {
      case call: FunCall =>

        val calls = getCallsAtLevel(call)

        calls.foreach(call => {
          call.f match {
            case m: AbstractMap =>
              apply(m.f.body, insideLoop || isLoop(m.loopVar.range))
            case r: AbstractPartRed =>
              apply(call.args.head, insideLoop)
              apply(r.f.body, insideLoop || isLoop(r.loopVar.range))
            case i: Iterate => apply(i.f.body, insideLoop || isLoop(i.indexVar.range))
            case toLocal(f) => apply(insideLoop, f)
            case toGlobal(f) => apply(insideLoop, f)
            case toPrivate(f) => apply(insideLoop, f)
            case l: Lambda => apply(l.body, insideLoop)
            case fp: FPattern => apply(fp.f.body, insideLoop)
            case _ =>
          }

          markLevel(calls, insideLoop)

          val mapLclOption = getMapLcl(call.f)
          if (mapLclOption.isDefined) {
            val mapLcl = mapLclOption.get

            val nestedMapLcl = Utils.getExprForPatternInCallChain(mapLcl.f.body,
              {
                case FunCall(MapLcl(_, _), _) =>
                case FunCall(Tuple(_) | Zip(_), args@_*)
                  if args.exists(
                    Utils.getIndexForPatternInCallChain(_, { case FunCall(MapLcl(_, _), _) =>}) != -1
                  ) =>
              })

            nestedMapLcl match {
              case Some(FunCall(innerMapLcl:MapLcl, _)) =>
                innerMapLcl.emitBarrier = false
              case Some(FunCall(_, args@_*)) =>
                args
                  .map(Utils.getExprForPatternInCallChain(_, { case FunCall(MapLcl(_, _), _) =>}))
                  .collect({ case Some(FunCall(decl:MapLcl, _)) => decl })
                  .foreach(_.emitBarrier = false)
              case _ =>
            }

            // TODO: Also gets rid of necessary barriers
//            if (mapLcl.f.body.addressSpace == PrivateMemory)
//              mapLcl.emitBarrier = false
          }

        })

      case _ =>
    }
  }

  // Helper method to bypass toAddressSpace and not visit whatever is inside twice
  private def apply(insideLoop: Boolean, f: Lambda1): Unit = {
    f.body match {
      case call: FunCall => call.f match {
        case fp: FPattern => apply(fp.f.body, insideLoop)
        case _ =>
      }
      case _ =>
    }
  }

  private def getCallsAtLevel(expr: Expr): Seq[FunCall] = {
    expr match {
      case call: FunCall =>
        call.f match {
          case Lambda(_, body,_) => getCallsAtLevel(body) ++ call.args.reverse.flatMap(getCallsAtLevel)
          case r:AbstractPartRed => call +: getCallsAtLevel(call.args(1))
          case _ => call +: call.args.reverse.flatMap(getCallsAtLevel)
        }
      case _ => Seq()
    }
  }

  private def isLoop(range: Range): Boolean = {

    // TODO: Information needed in several places
    // TODO: Information needed elsewhere. See analysis.ControlFlow
    // try to see if we really need a loop
    range.numVals match {
      case Cst(0) =>
        // zero iterations
        return false

      case Cst(1) =>
        return false

      // TODO: See TestInject.injectExactlyOneIterationVariable
      // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
      case _ if range.isInstanceOf[RangeAdd] && {
        val rangeAdd = range.asInstanceOf[RangeAdd]
        rangeAdd.start.min.min == Cst(0) &&
          ArithExpr.substituteDiv(rangeAdd.stop) == ArithExpr.substituteDiv(rangeAdd.step)
      }

      =>

        return false

      // TODO: See TestOclFunction.numValues and issue #62
      case _ if range.isInstanceOf[RangeAdd] && {
        val rangeAdd = range.asInstanceOf[RangeAdd]
        rangeAdd.start.min.min == Cst(0) && rangeAdd.stop == Cst(1)
      } =>
        return false
      case _ =>
        (range.numVals.min, range.numVals.max) match {
          case (Cst(0), Cst(1)) =>
            // one or less iteration
            return false

          case _ =>
        }
    }

    true
  }

  private def markLevel(calls: Seq[FunCall], insideLoop: Boolean): Unit = {

    val c = calls.count(_.isConcrete(false))

    var next = calls
    var groups = Seq[Seq[FunCall]]()

    if (c > 0) {

      // Partition the functions into groups such that the last element
      // of a group is concrete, except possibly in the last group
      while (next.nonEmpty) {
        val prefixLength = next.prefixLength(_.isAbstract(false))
        groups = groups :+ next.take(prefixLength + 1)
        next = next.drop(prefixLength + 1)
      }

      // If the last group it is not concrete, then there can't be a barrier
      if (groups.last.last.isAbstract)
        groups = groups.init

      val needsBarrier = Array.fill(groups.length)(false)

      val barrierInHead = groups.head.exists(c => isMapLcl(c.f))
      val finalReadFromLocal = readsFromLocal(groups.head.last)
      needsBarrier(0) =
        if (groups.length > 1)
          !(barrierInHead && (!finalReadFromLocal
            || finalReadFromLocal && !insideLoop))
        else
          (OpenCLMemory.containsLocalMemory(groups.head.last.mem) ||
            groups.head.last.args.foldLeft(false)((needsBarrier, arg) => {
              OpenCLMemory.containsLocalMemory(arg.mem) || needsBarrier
            })) && insideLoop

      groups.zipWithIndex.foreach(x => {
        val group = x._1
        val id = x._2

        if (argumentToPossibleSharing(group.last)) {
          needsBarrier(id) = true

         // If int local, also needs a barrier after being consumed
          if (OpenCLMemory.containsLocalMemory(group.last.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        if (possibleSharing(group.last) && id < groups.length - 1) {
          needsBarrier(id + 1) = true

          if (OpenCLMemory.containsLocalMemory(group.last.argsMemory))
            needsBarrier(id) = true
        }

        // Conservative assumption. TODO: Not if only has matching splits and joins
        if (group.exists(call => isPattern(call, classOf[Split]) || isPattern(call, classOf[Join])
           || isPattern(call, classOf[asVector]) || isPattern(call, classOf[asScalar]) || isPattern(call, classOf[Slide])) && id > 0) {
          needsBarrier(id) = true

          // Split/Join in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (OpenCLMemory.containsLocalMemory(group.last.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Scatter affects the writing of this group and therefore the reading of the
        // group before. Gather in init affects the reading of the group before
        if (group.exists(call => isPattern(call, classOf[Scatter]) || isPattern(call, classOf[Gather])
          || isPattern(call, classOf[Transpose]) || isPattern(call, classOf[TransposeW]) || isPattern(call, classOf[Slide])
          || isPattern(call, classOf[Tail])) && id > 0) {

          needsBarrier(id) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (OpenCLMemory.containsLocalMemory(group.last.mem) && id > 1 && insideLoop &&
            !groups.slice(0, id - 1).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id - 1) = true
        }

        // Gather in last affects the reading of this group
        if ((isPattern(group.last, classOf[Gather]) || isPattern(group.last, classOf[Transpose]) || isPattern(group.last, classOf[Slide])) && id < groups.length - 1) {
          needsBarrier(id + 1) = true

          // Reorder in local also needs a barrier after being consumed (two in total), if in a loop.
          // But it doesn't matter at which point.
          if (readsFromLocal(group.last) && id > 0 && insideLoop &&
            !groups.slice(0, id).map(_.exists(c => isMapLcl(c.f))).reduce(_ || _))
            needsBarrier(id) = true
        }

        // Sequential pattern following parallel pattern needs barrier after parallel section
        if (id < groups.length-1 && group.exists(x => isSequential(x.f)) && groups(id+1).exists(x => isMapLcl(x.f))) {
          needsBarrier(id+1) = true

          // TODO: Local memory? Can be overwritten before consumed but can't
          // TODO: currently have a barrier after a sequential pattern
        }

      })

      (groups, needsBarrier).zipped.foreach((group, valid) =>
        if (!valid) invalidateBarrier(group))
    }
  }


  private[generator] def isPattern[T](funCall: FunCall, patternClass: Class[T]): Boolean = {
    BarrierElimination.isPattern(funCall, patternClass)
  }

  private def argumentToPossibleSharing(call: FunCall): Boolean = {
    Expr.visitWithState(false)(lambda.body, {
      case (FunCall(Lambda(params, FunCall(_, nestedArgs@_*),_), args@_*), _ )
        if args.exists(arg => arg.contains({ case c if c eq call => } )) && !params.sameElements(nestedArgs)=>
        true
      case (_, state) => state
    })
  }

  // If a map calls its function with something other than
  // the parameter from its lambda or containing that
  // there can possibly be threads reading different elements
  private def possibleSharing(call: FunCall): Boolean = {
    call.f match {
      case m: AbstractMap =>
        m.f match {
          case Lambda(params, FunCall(_, args @ _*),_)
            if !params.sameElements(args) =>

            !Expr.visitWithState(false)(args.head, (e, b) =>
              if (e.eq(params.head)) true else b)

          case _ => false
        }
      case _ => false
    }
  }


  private def isMapLcl(funDecl: FunDecl): Boolean = {
    def isMapLclLambda(f: Lambda1): Boolean = {
      f.body match {
        case call: FunCall => isMapLcl(call.f)
        case _ => false
      }
    }

    funDecl match {
      case _: MapLcl => true
      case toLocal(f) => isMapLclLambda(f)
      case toGlobal(f) => isMapLclLambda(f)
      case toPrivate(f) => isMapLclLambda(f)
      case _ => false
    }
  }

  private def isSequential(funDecl: FunDecl): Boolean = {
    def isSequentialLambda(f: Lambda): Boolean = {
      f.body match {
        case call: FunCall => isSequential(call.f)
        case _ => false
      }
    }

    funDecl match {
      case _: MapSeq => true
      case _: ReduceSeq => true
      case toLocal(f) => isSequentialLambda(f)
      case toGlobal(f) => isSequentialLambda(f)
      case toPrivate(f) => isSequentialLambda(f)
      case _ => false
    }
  }

  private def getMapLcl(funDecl: FunDecl): Option[MapLcl] = {
    def getMapLclLambda(f: Lambda1): Option[MapLcl] = {
      f.body match {
        case call: FunCall => getMapLcl(call.f)
        case _ => None
      }
    }

    funDecl match {
      case m: MapLcl => Some(m)
      case toLocal(f) => getMapLclLambda(f)
      case toGlobal(f) => getMapLclLambda(f)
      case toPrivate(f) => getMapLclLambda(f)
      case _ => None
    }
  }

  private def invalidateBarrier(group: Seq[FunCall]): Unit = {
    group.foreach(invalidateBarrier)
  }

  private def invalidateBarrier(c: FunCall): Unit = {
    getMapLcl(c.f) match {
      case Some(b) => b.emitBarrier = false
      case None =>
    }
  }

  private def readsFromLocal(call: FunCall): Boolean = {
    val result = Expr.visitWithState(false)(call, (expr, addressSpace) => {
      expr match {
        case call: FunCall =>
          call.f match {
            case uf: UserFun =>
              if (OpenCLMemory.containsLocalMemory(call.argsMemory))
                true
              else
                addressSpace
            case _ => addressSpace
          }
        case _ => addressSpace
      }
    }, visitArgs = false)

    result
  }

}
