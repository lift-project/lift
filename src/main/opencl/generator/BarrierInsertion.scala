package opencl.generator

import com.typesafe.scalalogging.Logger
import core.generator.GenericAST.{ExpressionT, VarRef}
import ir.ast.{AbstractMap, AbstractPartRed, ArrayAccess, CheckedArrayAccess, Expr, FPattern, FunCall, FunDecl, Gather, Get, Head, Id, Join, Lambda, Pad, PadConstant, Pattern, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unslide, Unzip, UserFun, Zip, asScalar, asVector, debug}
import ir.interpreter.Interpreter.ValueMap
import ir.view._
import ir.{Memory, Type}
import lift.arithmetic.NotEvaluableException.NotEvaluable
import lift.arithmetic.{ArithExpr, Cst, RangeAdd, Var}
import opencl.generator.BarrierInsertion.Access.ControlFlowPathBetweenAccesses
import opencl.generator.BarrierInsertion.ParDomain.ParDomain
import opencl.generator.BarrierInsertion._
import opencl.ir.pattern._
import opencl.ir.{GlobalMemory, LocalMemory, OpenCLMemory, PrivateMemory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Barrier insertion.
 * The module splits the problem in four stages:
 * - Building memory access graph representing the order of memory accesses (writes and reads): `BuildMemoryAccessGraph`
 * - Inferring where barriers need to be placed: `inferBarriers`
 * - Removing duplicate barriers: `removeDuplicateBarriersFromGraph`
 * - Inserting barriers: `insertBarriers`
 *
 * The barrier inference has two goals: to put as few barriers as possible, and to put them as late in the
 * OpenCL kernel as possible.
 * - Fewer barriers is achieved by building control flow paths between access pairs that need synchronization and
 *   placing barriers on intersections of these paths
 * - Barriers are inserted as late as possible as a heuristic for performance, to issue as many blocking requests
 *   as possible before stopping to wait on a barrier.
 *   TODO: there might be better strategies in finding the best barrier locations that try to predict whether
 *   placing a barrier would prevent some optimizations happening on OpenCL compiler level. For example, in
 *   `buffer[idx] = ..; barrier; .. = f(buffer[idx])` the barrier might prevent the compiler from simplifying
 *   the two accesses.
 *
 * The barriers are set using the identity AST node Barrier. Decisions on barrier insertion are made
 * using views and index expressions rather than AST patterns since views contain all necessary information
 * in a more concise way. This module is meant to fix the problems to do with BarrierElimination:
 * - Some of the required barriers are missing:
 * - - MapGlb(1)(MapGlb(0)(Reduce(..)) o [view change] o MapGlb(0)(Map(..)))
 * - - - See src/test/resources/barrier_elimination_bugreports/mapglb_missing_barrier for the lambda and the kernel.
 * - - MapWrg(0)( Let( MapLcl(0)(..) ) o MapLcl(0))
 * - - - See src/test/resources/barrier_elimination_bugreports/maplcl_missing_barrier for the lambda and the kernel.
 * - - See test/opencl/generator/TestBarrierInsertion.[noLoopReorderLastLocal, noLoopReorder2Local] and other tests
 * - - of the suite for more examples.
 * - - - Some of these missing barriers are to do with writing after reading between loop iterations.
 *
 * - Some of the barriers are too deep in the inner loops, which might harm performance:
 * - - MapLcl() o [view change] o MapSeq(MapLcl())
 * - - - The barrier in src/test/resources/barrier_elimination_bugreports/maplcl_missing_barrier/kernel_391113.cl:102
 * - - - can be moved outside the for loop.
 *
 * - Some of the barriers left by barrier elimination are not needed:
 * - - See test/opencl/generator/TestBarrier.reorderLocal() and other tests of the suite for examples.
 *
 * By default, this module is disabled; see environment variables in CompilationSettings to enable the module and
 * ignore the emitBarrier flags controlled by BarrierElimination.
 */
class BarrierInsertion(lambda: Lambda,
                       localSize: NDRange,
                       globalSize: NDRange) {

  // TODO: Generalize to other platforms
  val warpSize = MaliBifrost.warpSize

  private val logger = Logger(this.getClass)

  val (memoryAccessGraph: Option[Access],
  memAccesses: Predef.Map[Memory, List[Access]],
  potentialBarrierLocationsInAST: Predef.Map[Expr, SynchronizableTransition]) =
    BuildMemoryAccessGraph(lambda)

  def apply(): Lambda = {
    if (memoryAccessGraph.isDefined) { // Are there memory accesses at all?
      inferBarriers()
      insertBarriers(lambda)
    } else {
      // No memory accesses => no barriers
      lambda
    }
  }

  private def inferBarriers(): Unit = {
    // Ordering of barrier inference matters.  We try to insert barriers as late as possible; it means that if we create
    // one barrier late in the control flow and then create second in the middle, we will have 2 barriers;
    // if we create the middle one first, then, during creation of the later one we will see that the
    // middle one suffices, and thus will keep only 1 barrier.
    // To avoid such problems, we will first extract all paths to be synchronized, and then insert
    // barriers on their intersections.
    val pathsToBeSynchronized = collectPathsToBeSynchronized

    /**
     * Place the barriers as late as possible in the OpenCL kernel;
     * place as few barriers as possible by inserting them into intersections of paths to be synchronized.
     */
    val criticalPathsToSync = filterCriticalSubpaths(pathsToBeSynchronized.map(_._2))

    val criticalPathsToSyncWithIntersects = countPathIntersections(criticalPathsToSync)

    /**
     * For each path, put a barrier on a transition with the maximum number of intersections.
     * Ties are broken using the order of access in AST: when two transitions have the same number of
     * intersections, one occurring later in the OpenCL kernel is chosen.
      */
    val maxNIntersectionsPerPath = criticalPathsToSyncWithIntersects.map(path =>
      if (path.nonEmpty) path.maxBy(_._3)._3 else 0)

    val ambiguousBarrierLocations =
      criticalPathsToSyncWithIntersects
        .zip(pathsToBeSynchronized.map(_._1))
        .zip(maxNIntersectionsPerPath)
        .map {
          case ((pathWithCounters, barrierDomain), max) => (pathWithCounters.filter(_._3 == max), barrierDomain)
        }

    var ambiguousBarrierLocationsUpd = propagateBarrierPlacementDecisions(ambiguousBarrierLocations)

    // Iteratively resolve all dependencies
    while (ambiguousBarrierLocationsUpd.nonEmpty) {
      /** Resolve one of the ambiguous locations heuristically **/
      resolveOneBarrierHeuristically(ambiguousBarrierLocationsUpd)

      ambiguousBarrierLocationsUpd = propagateBarrierPlacementDecisions(ambiguousBarrierLocationsUpd)
    }

    removeDuplicateBarriersFromGraph()
  }

  /**
   * Collects control flow paths between accesses to the same memory that require a barrier.
   * Later, we will find intersections between these paths to insert as few barriers as possible.
   */
  def collectPathsToBeSynchronized: List[(ParDomain, ControlFlowPathBetweenAccesses)] = {
    val pathsToBeSynchronized: ListBuffer[(ParDomain, ControlFlowPathBetweenAccesses)] = ListBuffer()

    /** Populate `pathsToBeSynchronized` **/
    for (mem <- memAccesses.keys.filter( !inputOrOutputMem(_))) {
      // For all possible pairs of mem accesses, check if barrier is needed
      val allMemAccessPairs = memAccesses(mem).combinations(2).flatMap(_.permutations)

      allMemAccessPairs.foreach {
        case access1 :: access2 :: Nil =>

          // Find a shortest sequence of accesses that starts with access1 and ends with access2,
          // and none of the accesses in-between access the memory of access1 and access2
          val shortestPath = Access.shortestPath(
            startNode = access1,
            target = access2,
            earlyStop = { case Access(m, _, _) if m eq mem => })

          shortestPath match {
            case Nil => // The accesses are completely independent, so no barrier needed
            case _ =>
              // Decide if barrier is needed
              barrierRequiredBetweenMemAccesses(mem.asInstanceOf[OpenCLMemory],
                access1.accessor, access2.accessor) match {
                case None => // No barrier required
                case Some(domain) => // Barrier required
                  pathsToBeSynchronized.append((domain, shortestPath))
              }
          }

        case _ => throw new IllegalArgumentException()
      }
    }

    pathsToBeSynchronized.toList
  }

  def inputOrOutputMem(mem: Memory): Boolean = {
    if (!memAccesses.contains(mem))
      throw new IllegalStateException("Missed a memory when building a memory access graph!")

    !memAccesses(mem).exists {
      // concrete (non-param) producer exists => not an input mem
      case Access(_, Producer(e), _) if e.isConcrete(visitArgs = false) => true
      case _ => false
    } || !memAccesses(mem).exists {
      // consumer exists => not an output mem
      case Access(_, _: Consumer, _) => true
      case _ => false
    }
  }

  /**
   * There are several indicators that a barrier is needed when producing or consuming memory in parallel.
   * This function checks them in the increasing order of inference difficulty.
   */
  def barrierRequiredBetweenMemAccesses(mem: OpenCLMemory,
                                        accessor1: Accessor,
                                        accessor2: Accessor): Option[ParDomain] = {
    val parDomain: ParDomain = mem.addressSpace match {
      case LocalMemory => ParDomain.Local
      case GlobalMemory => ParDomain.Global
      case PrivateMemory => return None
      case _ => throw new IllegalArgumentException
    }

    // For two consecutive reads, the order of threads is not important
    (accessor1, accessor2) match {
      case (_: Consumer, _: Consumer) => return None
      case _ =>
    }

    //    val accessSubViews = View.getSubViews(lastParProducer.expr.outputView) ++ View.getSubViews(firstParConsumer.arg.view)
    val accessSubViews = //try {
      View.getSubViews(accessor1.view) ++ View.getSubViews(accessor2.view)
    /*} catch {
      case e =>
        println(e)
        println(mem)
        println(accessor1)
        println(accessor1.view)
        println(accessor2)
        println(accessor2.view)
        throw e
    }*/

    // Was the view transformed on mem production or consumption?
    val viewChanged = accessSubViews.exists {
      case _: ViewFilter |
           _: ViewSplit | _: ViewSlide | _: ViewJoin |
           _: ViewOffset | _: ViewSlice | _: ViewHead | _: ViewTail |
           _: ViewPad | _: ViewPadConstant |
           _: ViewReorder | _: ViewTranspose |
           _: ViewAsVector | _: ViewAsScalar => true
      case _ => false
    }

    val barrierRequired = if (!viewChanged) {
      // The view wasn't changed, so now we can check the nesting of maps, i.e. MapSeq(MapLcl()) o MapLcl(MapSeq())
      // This check is faster/easier than that of the full index expressions below.
      // If we cannot prove that the iterators have the same ranges (in constant terms, or
      // in terms of the same functions such as get_local_id), insert a barrier.
      //      !producerConsumerAccessDimensionalitiesAreStructEqual(lastParProducer.expr, firstParConsumer.arg, parDomain)
      !accessStacksAreStructEqual(
        accessor1.expr, accessor2.expr, parDomain)

    } else {
      // Perform a more thorough check of accesses using full index expressions
      val secondAccessRangeIncludedInFirst: Boolean = {

        val access1idx = cAccessToArithExpr(ViewPrinter.emit(accessor1.view))
        val access2idx = cAccessToArithExpr(ViewPrinter.emit(accessor2.view))

        val (access1idxSimplified, access2IdxSimplified) =
          simplifyParIndicesIteratingWithinSingleWarp(access1idx, access2idx)

        parallelIndicesAcrossWarpsAreStructEqual(access1idx, access2idx) && {
          try {
            access2IdxSimplified.rangeIncludedInRangeOf(access1idxSimplified)
          } catch {
            case NotEvaluable =>
              logger.warn("Cannot prove whether a thread accesses only the data it produced. Inserting barrier.")
              false
          }
        }
      }

      // If we cannot prove that the second access range is included in that of the first, insert a barrier
      !secondAccessRangeIncludedInFirst
    }

    if (barrierRequired) Some(parDomain)
    else None
  }

  def accessStacksAreStructEqual(accessor1: Expr,
                                 accessor2: Expr,
                                 parDomain: ParDomain): Boolean = {
    val accessIterators = parDomain match {
      case ParDomain.Local =>
        val access1Iterators = accessor1.accessInf.localAccessInf.map(_._2)
        val access2Iterators = accessor2.accessInf.localAccessInf.map(_._2)

        // If the two accessors' access depths are different, the memory is accessed through
        // different levels of parallelization.
        if (access1Iterators.length != access2Iterators.length)
          return false
        else access1Iterators.zip(access2Iterators)

      case ParDomain.Global =>
        val access1Iterators = accessor1.accessInf.globalAccessInf.map(_._2)
        val access2Iterators = accessor2.accessInf.globalAccessInf.map(_._2)

        // If producer and consumer's access depths are different, the memory is accessed through
        // different levels of parallelization.
        if (access1Iterators.length != access2Iterators.length)
          return false
        else access1Iterators.zip(access2Iterators)

      case _ => throw new IllegalStateException()
    }

    accessIterators.foldLeft(true) {
      case (othersEqual, (producerAccessVar, consumerAccessVar)) =>
        othersEqual && producerAccessVar.equalsStructurally(consumerAccessVar)
    }
  }

  /**
   * Filter the paths to be synchronized to remove the transitions where barriers cannot or should not be inserted
   */
  def filterCriticalSubpaths(paths: List[ControlFlowPathBetweenAccesses]
                            ):      List[List[(Access, CriticalTransition)]] = {
    val criticalSubpaths = paths.map(_.collect {
      case (access, Some(t @ CriticalTransition(_, _, insideVarLenMapLcl @ false))) =>
        (access, t)
    })
    // This check covers one performed by the later CheckBarriersAndLoops
    paths.zip(criticalSubpaths).foreach {
      case (path, criticalPath) =>
        if (criticalPath.isEmpty) {
          val exprInsideVarLenMapLcl = path.collectFirst {
            case (_, Some(CriticalTransition(_, syncPointInAST, insideVarLenMapLcl @ true))) => syncPointInAST
          }

          exprInsideVarLenMapLcl match {
            case Some(e) =>
              val varLenMapLcl = getVarLenMapLcl(e)
              throw new IllegalKernel("Kernel requires a barrier that might not be taken by all threads inside\n"
                + varLenMapLcl)

            case None =>
              throw new IllegalArgumentException("Could not find a valid location to insert a barrier between " +
                s"${path.head._1.accessor} and ${path.last._1.accessor}")
          }
        }
    }
    criticalSubpaths
  }

  def getVarLenMapLcl(exprInsideVarLenMapLcl: Expr): MapLcl = {
    var varLenMapLcl: Option[MapLcl] = None
    Expr.visit(lambda.body, {
      case FunCall(m: MapLcl, _)
        if ((m.iterationCount.min !== m.iterationCount.max) ||
          !m.iterationCount.min.isEvaluableGivenEnoughExtraData ||
          !m.iterationCount.max.isEvaluableGivenEnoughExtraData ) &&
          m.f.body.contains({
            case e if e eq exprInsideVarLenMapLcl =>
          }, visitArgs = true) =>
        varLenMapLcl = Some(m)

      case _ =>
    }, {_ => Unit})

    if (varLenMapLcl.isEmpty)
      throw new IllegalStateException()

    varLenMapLcl.get
  }

  /**
   * Add a counter to each transition in all paths denoting the number of paths to be synchronized
   * that pass through the transition
   */
  def countPathIntersections(paths: List[List[(Access, CriticalTransition)]]
                            ):      List[List[(Access, CriticalTransition, Int)]] =
    paths.map(_.map {
      case (access, transition) => (access, transition, paths.count(_.contains(transition)))
    })

  def barrierWithOnlyOneLocationExists(barrierLocations: List[(List[(Access, CriticalTransition, Int)], ParDomain)]
                                      ): Boolean =
    barrierLocations.exists(_._1.length == 1)

  /**
   * For some paths, the barrier can be placed in only one location; the function sets these barriers.
   * For some paths, a barrier might already exist -- the function removes these paths from the return list.
   * @return remaining ambiguous barrier locations
   */
  def propagateBarrierPlacementDecisions(barrierLocations: List[(List[(Access, CriticalTransition, Int)], ParDomain)]
                                        ): List[(List[(Access, CriticalTransition, Int)], ParDomain)] = {
    var anotherPropagationRequired = true
    var barriersLocationsToPropagate = barrierLocations

    while (anotherPropagationRequired) {
      anotherPropagationRequired = false

      barriersLocationsToPropagate = barriersLocationsToPropagate.flatMap {
        // Barrier already exists:
        case (possibleBarrierLocations, barrierDomain)
          if possibleBarrierLocations.exists(loc => loc._2.barrier.isDefined &&
            loc._2.barrier.get.parDomains.contains(barrierDomain)) =>
          None // Not an ambiguous path anymore

        // A simple unambiguous location:
        case (possibleBarrierLocations, barrierDomain)
          if possibleBarrierLocations.length == 1 =>
          setBarrierOnTransition(possibleBarrierLocations.head._2, barrierDomain)
          anotherPropagationRequired = true
          None // Not an ambiguous path anymore

        // Still an ambiguous location:
        case (possibleBarrierLocations, barrierDomain) =>
          Some((possibleBarrierLocations, barrierDomain))
      }
    }

    barriersLocationsToPropagate
  }

  /**
   * For all control flow paths, there is more than one barrier location.
   * Place one barrier heuristically as close as possible to the end of OpenCL kernel
   */
  def resolveOneBarrierHeuristically(barrierLocations: List[(List[(Access, CriticalTransition, Int)], ParDomain)]
                                    ): Unit = {

    val furthestBarrierLocationInAST = barrierLocations.foldLeft[Int](-1) {
      case (furthestObservedLocation, (path, _)) =>
        val furthestLocationInPath = if (path.nonEmpty) path.map(_._1.orderInAST).max else -1
        Math.max(furthestObservedLocation, furthestLocationInPath)
    }

    val oneBarrierSet = barrierLocations.flatMap { // package with barrier domains
      case (locations, domain) => locations.map(l => (l._1, l._2, l._3, domain))
    }.collectFirst { // set one barrier in the furthest location
      case (access, transition, _, domain)
        if access.orderInAST == furthestBarrierLocationInAST =>
        setBarrierOnTransition(transition, domain)
    }
    assert(oneBarrierSet.isDefined)
  }

  def setBarrierOnTransition(t: CriticalTransition,
                             domain: ParDomain): Unit = {
    if (t.barrier.isEmpty)
      t.barrier = Some(MutableBarrier(mutable.Set(domain)))
    else if (!t.barrier.get.parDomains.contains(domain))
      t.barrier.get.parDomains.add(domain)
  }

  /**
   * Removes duplicate barriers that might occur on edges (transitions) of the same node (access).
   * The reason for their occurrence is that when placing a barrier the first time, we only consider the path --
   * the chain of accesses and transitions -- between two accesses that need synchronization. We do not
   * consider the transitions starting on the accesses of the path that do not lead to accesses on the path.
   * This might result in cases such as `Barrier o Map(Barrier o f)`, where the outer barrier can always be
   * removed (the map is either not MapLcl or MapLcl numIters.min == numIters.max, hence it's save to remove
   * the outer barrier).
   */
  def removeDuplicateBarriersFromGraph(): Unit = {
    memAccesses.values.flatten.toSet.foreach((access: Access) =>
      access.transitions.foldLeft[mutable.Set[ParDomain]](mutable.Set()) {
        case (precedingBarrierDomains, transition @ CriticalTransition(Some(barrier), _, _)) =>
          val originalBarrierDomains = barrier.parDomains
          val duplicateBarrierDomains = precedingBarrierDomains.intersect(originalBarrierDomains)
          // Remove the duplicate domains from the barrier
          duplicateBarrierDomains.foreach(domain =>
            barrier.parDomains.remove(domain))
          // Remove the barrier if possible
          if (barrier.parDomains.isEmpty)
            transition.barrier = None

          precedingBarrierDomains ++ originalBarrierDomains
        case (collectedBarrierDomains, _) => collectedBarrierDomains
      }
    )
  }

  private def insertBarriers(f: FunDecl): FunDecl = f match {
    case fp: FPattern => fp.copy(Lambda(fp.f.params, insertBarriers(fp.f.body)))
    case l: Lambda    => Lambda(l.params, insertBarriers(l.body))
    case f            => f
  }

  private def insertBarriers(expr: Expr): Expr = {
    val exprWithPotentialBarriers = expr match {
      case call: FunCall =>
        val argsWithPotentialBarriers = call.args.map(insertBarriers)
        val fWithPotentialBarriers = insertBarriers(call.f)

        FunCall(fWithPotentialBarriers, argsWithPotentialBarriers: _*)

      case e => e
    }

    if (potentialBarrierLocationsInAST.contains(expr) &&
      potentialBarrierLocationsInAST(expr).barrier.isDefined) {
      val local = potentialBarrierLocationsInAST(expr).barrier.get.parDomains.contains(ParDomain.Local)
      val global = potentialBarrierLocationsInAST(expr).barrier.get.parDomains.contains(ParDomain.Global)

      FunCall(Barrier(local, global), exprWithPotentialBarriers)
    } else
      exprWithPotentialBarriers
  }


  /**
   * Checks if the structural difference between two arithexprs contains a parallel component.
   * Only considers parallel indices in local/global dimensions with sizes larger than warp size.
   * (Motivation here is that threads within a warp do not need synchronization, e.g. with warp size of 4,
   * if thread 0 depends on the results produced by only its own warp, no barrier is needed)
   * // TODO: generalize for multiple warps, i.e. where quad threads are interdependent and there is more than one warp
   */
  def parallelIndicesAcrossWarpsAreStructEqual(access1: ArithExpr, access2: ArithExpr): Boolean = {
    val diffs: List[ArithExpr] = access1.structuralDiff(access2) match {
      case None | Some((None, None)) => List()
      case Some((Some(diff1), None)) => List(diff1)
      case Some((None, Some(diff2))) => List(diff2)
      case Some((Some(diff1), Some(diff2))) => List(diff1, diff2)
    }
    diffs.foreach(diff => {
      var groupIdxFound = false
      val localDomainsInDiff = ListBuffer[Int]()
      val globalDomainsInDiff = ListBuffer[Int]()

      diff.visitAndRebuild({

        case oFun: OclFunction
          if oFun.name.equals("get_local_id") =>
          localDomainsInDiff += oFun.param
          oFun

        case oFun: OclFunction
          if oFun.name.equals("get_group_id") =>
          groupIdxFound = true
          oFun

        case oFun: OclFunction
          if oFun.name.equals("get_global_id") =>
          globalDomainsInDiff += oFun.param
          oFun
        case ae => ae
      })

      if (groupIdxFound)
        return false

      if (localDomainsInDiff.nonEmpty && {
          // We need to check that threads in all par dims preceding current one fit into one warp exactly.
          // E.g. for dim 1 of size 3 and dim 0 of size 4, and checking dim 1, we need to check that 3*4 == warpSize
          // Note that if all threads in the relevant dims don't fill the warp completely, the warp is spread across
          // higher dimensions, which means we need a barrier.
          // E.g. for local sizes of (1,3,16) and localDomainsInDiff being 0 and 1, and the two accesses write and
          // read 48 elements for each local id of 2, the first warp of four threads
          // has threads (0,0,0),(0,1,0),(0,2,0),(1,0,0), and the second warp has (1,1,0),(1,2,0),(2,0,0),(2,1,0), i.e.
          // the threads with local_id_2 of 1 depend on data produced by two warps, hence we need to sync across dim 2 as well.
          val totalSizeOfAllDimsUpToTheMaxOne = (0 to localDomainsInDiff.max).foldLeft[ArithExpr] (Cst(1)) {
            case (mult, d) => mult * localSize(d) }

        Cst(warpSize) != totalSizeOfAllDimsUpToTheMaxOne
      })
        return false

      if (globalDomainsInDiff.nonEmpty && {
        val totalSizeOfAllDimsUpToTheMaxOne = (0 to globalDomainsInDiff.max).foldLeft[ArithExpr] (Cst(1)) {
          case (mult, d) => mult * localSize(d) }

        Cst(warpSize) != totalSizeOfAllDimsUpToTheMaxOne
      })
        return false
    })

    true
  }

  // TODO: generalize for multiple warps, i.e. where quad threads are interdependent and there is more than one warp
  def simplifyParIndicesIteratingWithinSingleWarp(access1: ArithExpr, access2: ArithExpr): (ArithExpr, ArithExpr) = {

    def parDimConfinedToSingleWarp(dim: Int, dimSizes: NDRange): Option[Boolean] =
      ArithExpr.isSmaller(dimSizes(dim), warpSize) match {
        case Some(b) => Some(b || dimSizes(dim) == Cst(warpSize))
        case None => None
      }

    // Var("l_id", RangeAdd(get_local_id, iters_stop, <group_size>)) ->
    // Var("l_id", RangeAdd(Var(0..<group_size>), iters_stop, <group_size>)) ->
    def simplify(access: ArithExpr): ArithExpr =
      access.visitAndRebuild({
        case Var(vName, RangeAdd(oFun: OclFunction, stop, step))
          if oFun.name.equals("get_local_id") &&
            parDimConfinedToSingleWarp(
              dim = oFun.param, dimSizes = localSize).getOrElse(false) =>
          assert(step == localSize(oFun.param))

          Var(vName, RangeAdd(0, stop, 1))

        case Var(vName, RangeAdd(oFun: OclFunction, stop, step))
          if oFun.name.equals("get_global_id") &&
            parDimConfinedToSingleWarp(
              dim = oFun.param, dimSizes = globalSize).getOrElse(false) =>
          assert(step == globalSize(oFun.param))

          Var(vName, RangeAdd(0, stop, 1))

        case ae => ae
      })

    (simplify(access1), simplify(access2))
  }
}

object BarrierInsertion {
  def apply(lambda: Lambda,
            localSize: NDRange,
            globalSize: NDRange): Lambda = {
    val pass = new BarrierInsertion(lambda, localSize, globalSize)
    val result = pass.apply()
    result
  }

  def cAccessToArithExpr(access: ExpressionT): ArithExpr = {
    access match {
      case v: VarRef =>
        assert(v.arrayIndex.isDefined)

        v.arrayIndex.get.content

      case _ => throw new NotImplementedError()
    }
  }

  def accessorParallelization(e: Expr): List[ParDomain] = {
    val callSubViews = View.getSubViews(e.view)
    val producerParDomains: ListBuffer[ParDomain] = ListBuffer()

    callSubViews.foreach {
      case ViewAccess(i, _, _) =>
        i.visitAndRebuild({ // Not actually rebuilding here, just visiting. For the lack of simpler visitor
          case oFun: OclFunction if oFun.name.equals("get_local_id") =>
            producerParDomains.append(ParDomain.Local)//LocalDim(oFun.param))
            oFun
          case oFun: OclFunction if oFun.name.equals("get_group_id") =>
            producerParDomains.append(ParDomain.Group) //WorkgroupDim(oFun.param))
            oFun
          case oFun: OclFunction if oFun.name.equals("get_global_id") =>
            producerParDomains.append(ParDomain.Global) //GlobalDim(oFun.param))
            oFun
          case ae => ae
        })
      case _ =>
    }

    producerParDomains.toList
  }

  /**
   * Memory access graph is a directed graph representing the order in which buffers
   * are accessed in the control flow. The edges of the graph are represented
   * using `transitions` representing the passage of the control flow between accesses;
   * each transition might have a barrier associated with it.
   *
   * For example, `g $ Map(f(a, b))` has the following accesses and transitions:
   * - a_a:    Access(mem_a, accessor=Consumer(f(a,b)), transitions=[SynchronizableTransition(a_b)])
   * - a_b:    Access(mem_b, accessor=Consumer(f(a,b)), transitions=[Transition(a_f_wr)])
   * - a_f_wr: Access(mem_f, accessor=Producer(f(a,b)), transitions=[CriticalTransition(a_a), CriticalTransition(a_f_rd)])
   * - a_f_rd: Access(mem_f, accessor=Consumer(g $ ..), transitions=[Transition(a_g_wr)])
   * - a_g_wr: Access(mem_g, accessor=Produce(g $ ..), transition=[])
   *
   * The memory access graph is built by `BuildMemoryAccessGraph`; the barriers are inferred and inserted
   * by `inferBarriers` and `insertBarriers` respectively.
   *
   * The barriers are recorded on edges and not on nodes to represent control flow more accurately;
   * if an access was performed in a nest of two loops and a barrier already existed outside the outer
   * loop, there would be no way to record the barrier after the inner loop if the barriers were
   * recorded on nodes.
   */
  case class Access(mem: Memory,
                    accessor: Accessor,
                    orderInAST: Int) {
    val transitions: ListBuffer[Transition] = ListBuffer()

    override def equals(obj: Any): Boolean = obj match {
      case other: Access => this eq other
      case _ => false
    }
  }

  /**
   * A transition is a control flow path between two accesses with no other accesses in-between.
   * SynchronizableTransition is one where a barrier can be placed; CriticalTransition is one where a
   * barrier might be needed because the transition is on the path between two accesses that need synchronization/
   * A SynchronizableTransition that is not a CriticalTransition will never host a barrier.
   */
  case class Transition(nextAccess: Access) {
    def sync: SynchronizableTransition = this match {
      case s: SynchronizableTransition => s
      case _ => throw new IllegalArgumentException()
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: Transition => this eq other
      case _ => false
    }
  }

  /**
   * Where barriers can be inserted. Unless it also extends CriticalTransition,
   * synchronization is not needed in this transition
   * `syncPointInAST` contains the expression that governs the transition. For example:
   * - In `g $ h`, the `syncPointInAST` of the transition between write of `h` and read of `g.arg` is `h`.
   * - In `Map(f)`, the `syncPointInAST` of the transition between write of `f` and read of `f.arg` is `f`.
   *   (The transition between the read of `f.arg` and the write of is not a `SynchronizableTransition` since
   *    there is no way to place a barrier in-between those accesses (`mem_f = f(arg)`) in OpenCL)
    */
  trait SynchronizableTransition extends Transition {
    var barrier: Option[MutableBarrier]
    var syncPointInAST: Expr
    // A variable-length MapLcl is one whose iteration.min != iteration.max,
    // and thus barriers cannot be placed inside
    val insideVarLenMapLcl: Boolean
  }

  object SynchronizableTransition {
    def unapply(arg: SynchronizableTransition): Option[Option[MutableBarrier]] = Some(arg.barrier)
  }

  /**
   * Where synchronization might be needed at all.
   * In f(a, b), where a and b are data-independent, transition between a and b is not critical.
   */
  trait CriticalTransition extends SynchronizableTransition

  object CriticalTransition {
    def unapply(arg: CriticalTransition): Option[(Option[MutableBarrier], Expr, Boolean)] =
      Some((arg.barrier, arg.syncPointInAST, arg.insideVarLenMapLcl))
  }

  object Access {
    type ControlFlowPathBetweenAccesses = List[(Access, Option[Transition])]

    /**
     * @return shortest path between startNode and target represented by a sequence of
     *         Accesses joined by Transitions. The first access in the chain is startNode;
     *         the last access is target and it doesn't have a transition.
     *         None of the nodes of the path match earlyStop.
     *         Empty if a path doesn't exist.
     */
    def shortestPath(startNode: Access,
                     target: Access,
                     earlyStop: PartialFunction[Access, Unit]/*,
                     nodeToAvoid: Option[Access]*/): ControlFlowPathBetweenAccesses = {

      val visitedAccesses = ListBuffer[Access]()

      def findNodeRecursively(node: Access,
                              traversedNodes: ControlFlowPathBetweenAccesses
                             ): ControlFlowPathBetweenAccesses = {
        if (node equals target) { // Reached target?

          assert(!visitedAccesses.exists(_ equals node))
          visitedAccesses.append(node) // Mark as visited

          traversedNodes :+ (node, None)

        } else if (!(node eq startNode) && earlyStop.isDefinedAt(node))
          List()
        else if (visitedAccesses.exists(_ equals node)) // Visited this node before? Prevents infinite recursion
          List()
        else {
          visitedAccesses.append(node) // Mark as visited

          if (node.transitions.isEmpty) // Reached the end of the access chain?
            List()
          else {
            // Recurse into next nodes
            node.transitions.foldLeft[ControlFlowPathBetweenAccesses](List()) {

              case (Nil, transition) =>
                findNodeRecursively(transition.nextAccess, traversedNodes = traversedNodes :+ (node, Some(transition)))

              // Shorter path was already found, so nothing else to do
              case (previouslyFoundPath, transition) => previouslyFoundPath

            }
          }
        }
      }

      findNodeRecursively(startNode, traversedNodes = List())
    }
  }

  class BuildMemoryAccessGraph(expr: Expr) {

    val memAccesses: mutable.Map[Memory, ListBuffer[Access]] = mutable.Map()
    val potentialBarrierLocationsInAST: mutable.Map[Expr, SynchronizableTransition] = mutable.Map()

    private var accessOrderInAST: Int = 0
    private def getOrder: Int = {
      val oldOrder = accessOrderInAST
      accessOrderInAST += 1
      oldOrder
    }

    def apply(): (Option[Access],
      Predef.Map[Memory, List[Access]],
      Predef.Map[Expr, SynchronizableTransition]) = {
      val accesses = applyPartially(expr, insideMapLcl = false, insideVarLenMapLcl = false)
      (accesses.headOption,
        Predef.Map(memAccesses.toSeq.map(p => (p._1, p._2.toList)): _*),
        Predef.Map(potentialBarrierLocationsInAST.toSeq: _*))
    }

    // Storing accesses in lists in addition to linking through Access.transitions to
    // be able to link the last and first access of `f` in `Map(f)` or `Reduce(f)`.
    // If we tried to identify the last access of `f` by following the chain of transitions,
    // we could go in an infinite loop.
    private def applyPartially(expr: Expr,
                               insideMapLcl: Boolean,
                               insideVarLenMapLcl: Boolean): List[Access] = {
      expr match {
        case call: FunCall =>

          val indirectArgAccesses: Seq[List[Access]] = call.args.map(applyPartially(_, insideMapLcl, insideVarLenMapLcl))

          val funAccesses = call.f match {
            case uf: UserFun =>
              // The base case, one which instantiates Accesses

              // Here, we create arg accesses performed by the UserFun. If the args are not just
              // input memories but complex expressions themselves, their preceding accesses were
              // collected in indirectArgAccesses and concatenated below.
              val directArgAccesses = concatAccessSequences(
                accessSeqs = call.args.map(arg => List(Access(arg.mem, Consumer(call, arg), orderInAST = getOrder))),
                // Concatenate with transitioningExprs, meaning that there might be barriers between args
                transitioningExprs = Some(call.args), insideVarLenMapLcl)

              val bodyMemWrite = Access(call.mem, Producer(call), orderInAST = getOrder)

              val allUserFunAccesses = concatAccessSequences(
                accessSeqs = (directArgAccesses :+ bodyMemWrite).map(List(_)),
                // No transitioningExprs since there cannot be a barrier between a/b and f in f(a, b)
                transitioningExprs = None, insideVarLenMapLcl)

              recordMemAccesses(allUserFunAccesses)
              allUserFunAccesses

            case _: AbstractMap | _: AbstractPartRed =>
              getLoopBodyAccesses(call, loopBody = call.f.asInstanceOf[FPattern].f.body, insideMapLcl, insideVarLenMapLcl)

            case mv: MapSeqVector =>
              concatAccessSequences(
                accessSeqs =
                  (if (mv.vectorPartNonEmpty)
                    Seq(getLoopBodyAccesses(call, loopBody = mv.fVectorized.body, insideMapLcl, insideVarLenMapLcl))
                  else Seq()) ++
                    (if (mv.scalarPartNonEmpty)
                      Seq(getLoopBodyAccesses(call, loopBody = mv.fScalar.body, insideMapLcl, insideVarLenMapLcl))
                    else Seq()),
                // TODO: confirm correctness of transitioningExprs here
                transitioningExprs = Some(
                  (if (mv.vectorPartNonEmpty)
                    Seq(mv.fVectorized.body)
                  else Seq()) ++
                    (if (mv.scalarPartNonEmpty)
                      Seq(mv.fScalar.body)
                    else Seq())), insideVarLenMapLcl)

            // Abstract patterns not affecting between-memory access order
            case Barrier(_, _) | Unzip() | Zip(_) | Get(_) | Transpose() | TransposeW() | asVector(_) |
                 asScalar() | Split(_) | Join() | Scatter(_) | Gather(_) |
                 Pad(_,_,_) | PadConstant(_, _, _) | Tuple(_) | Slide(_,_) | Unslide(_,_) | Head() | Tail() |
                 debug.PrintType(_, _) | debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _, _) |
                 UnsafeArrayAccess(_) | CheckedArrayAccess(_) | ArrayAccess(_) | Id() => List()

            case l: Lambda      => applyPartially(l.body, insideMapLcl, insideVarLenMapLcl)
            case toPrivate(f)   => applyPartially(f.body, insideMapLcl, insideVarLenMapLcl)
            case toLocal(f)     => applyPartially(f.body, insideMapLcl, insideVarLenMapLcl)
            case toGlobal(f)    => applyPartially(f.body, insideMapLcl, insideVarLenMapLcl)

            case _              => throw new NotImplementedError()
          }

          val concatenatedArgAccessSeqs = concatAccessSequences(
            accessSeqs = indirectArgAccesses,
            // For each pair of arguments, the expr on the transition between the last access of the
            // first sequence and the first access in the second sequence is the first argument of the pair.
            // E.g. in f(a, b), the barrier between a and b can be inserted as follows: f(barrier $ a, b)
            transitioningExprs = Some(call.args), insideVarLenMapLcl)

          val result = concatAccessSequences(
            accessSeqs = Seq(concatenatedArgAccessSeqs, funAccesses),
            transitioningExprs = Some(Seq(call.args.last, call)), insideVarLenMapLcl)

          result

        case _ => List()
      }
    }

    private def getLoopBodyAccesses(call: FunCall,
                                    loopBody: Expr,
                                    insideMapLcl: Boolean,
                                    insideVarLenMapLcl: Boolean): List[Access] = {
      val bodyAccesses = applyPartially(loopBody,
        insideMapLcl = insideMapLcl || call.f.isInstanceOf[MapLcl],
        insideVarLenMapLcl = insideVarLenMapLcl ||
          (call.f match {
            case m: MapLcl
              // If bounds are not equal, or if they are not evaluable
              if (m.iterationCount.min !== m.iterationCount.max) ||
                !m.iterationCount.min.isEvaluableGivenEnoughExtraData ||
                !m.iterationCount.max.isEvaluableGivenEnoughExtraData =>
              true
            case _ => false
          }))

      if (bodyAccesses.nonEmpty) {

        if (loopBody.contains({ case FunCall(_: MapWrg, _) => }) || // Case 1
          !(insideMapLcl || call.f.isInstanceOf[MapLcl])            // Case 2
        ) {
          // Case 1: A special case, one where barrier cannot be placed outside MapWrg / MapGlb.
          // Case 2: We are not inside a MapLcl and the current map is not MapLcl. The outermost point at which
          //    we might want a barrier is right outside the outermost MapLcl, e.g. MapWrg(MapSeq( Barrier o MapLcl(f) ))
          //    Otherwise, we could insert a barrier outside a sequential loop nesting the mapLcl, and the threads will
          //    iterate through MapLcl without hitting the barrier: MapWrg(Barrier o MapSeq(MapLcl(f)) )

          // Create a transition, but not a synchronizable one
          val newTransition = Transition(nextAccess = bodyAccesses.head)
          bodyAccesses.last.transitions.append(newTransition)
          // Don't record the call in `potentialBarrierLocationsInAST` because, well, it's not

        } else {
          // MapLcl, MapSeq, ReduceSeq, etc
          if (bodyAccesses.last.transitions.nonEmpty &&
            (bodyAccesses.last.transitions.last.nextAccess eq bodyAccesses.head)) {
            // Inner loop. The synchronizable AST point needs to be "raised" to be the outer map
            // Remove
            potentialBarrierLocationsInAST -= bodyAccesses.last.transitions.last.sync.syncPointInAST
            // Add
            potentialBarrierLocationsInAST += (/*call*/loopBody -> bodyAccesses.last.transitions.last.sync)
            bodyAccesses.last.transitions.last.sync.syncPointInAST = /*call*/loopBody
          } else {
            // New transition that might need a barrier
            val newTransition: Transition with CriticalTransition =
              new Transition(nextAccess = bodyAccesses.head) with CriticalTransition {
                override var barrier: Option[MutableBarrier] = None
                override var syncPointInAST: Expr = /*call*/loopBody
                override val insideVarLenMapLcl: Boolean = insideVarLenMapLcl
              }
            potentialBarrierLocationsInAST += (/*call*/loopBody -> newTransition)
            bodyAccesses.last.transitions.append(newTransition)
          }
        }
      }
      bodyAccesses
    }

    private def recordMemAccesses(accesses: List[Access]): Unit = {
      accesses.foreach(access => {
        if (!memAccesses.contains(access.mem))
          memAccesses(access.mem) = ListBuffer[Access]()

        memAccesses(access.mem).append(access)
      })
    }

    private def concatAccessSequences(accessSeqs: Seq[List[Access]],
                                      transitioningExprs: Option[Seq[Expr]],
                                      nestedInVarLenMapLcl: Boolean): List[Access] = {
      if (accessSeqs.isEmpty)
        List()
      else if (accessSeqs.length == 1)
        accessSeqs.head
      else {
        val transExprsRepackaged: Seq[Option[Expr]] = transitioningExprs match {
          case None => accessSeqs.map(_ => None)
          case Some(exprs) =>
            assert(exprs.length == accessSeqs.length)
            exprs.map(Some(_))
        }

        val seqsWithExprs = accessSeqs.zip(transExprsRepackaged)


        seqsWithExprs.zip(seqsWithExprs.tail).foldLeft[List[Access]](List()) {

          case (concatenatedSequences, ((accessSeq1, _), (accessSeq2, _)))
            if accessSeq1.isEmpty || accessSeq2.isEmpty =>
            // Not much to do here
            concatenatedSequences ++ accessSeq1 ++ accessSeq2

          case (concatenatedSequences, ((accessSeq1, None), (accessSeq2, _))) =>

            // No transition expression, so the access expressions must be those of function args and
            // those of the function write mem. There cannot be a barrier between `f` and `a` or `b` in f(a, b).
            accessSeq1.last.transitions.append(Transition(nextAccess = accessSeq2.head))
            concatenatedSequences ++ accessSeq1 ++ accessSeq2

          case (concatenatedSequences, ((accessSeq1, Some(transExpr1)), (accessSeq2, _))) =>

            val newTransition: Transition with SynchronizableTransition =
              if (accessSeq1.map(_.mem).intersect(accessSeq2.map(_.mem)).isEmpty) {
                // Independent sequences since there is no memory that is accessed in both sequences.
                // Hence, not a critical transition (barrier will not be inserted)
                new Transition(nextAccess = accessSeq2.head) with SynchronizableTransition {
                  override var barrier: Option[MutableBarrier] = None
                  override var syncPointInAST: Expr = transExpr1
                  override val insideVarLenMapLcl: Boolean = nestedInVarLenMapLcl
                }
              } else {
                // There is a mem that's accessed by both sequences, so synchronization might be
                // needed => this is a critical transition
                new Transition(nextAccess = accessSeq2.head) with CriticalTransition {
                  override var barrier: Option[MutableBarrier] = None
                  override var syncPointInAST: Expr = transExpr1
                  override val insideVarLenMapLcl: Boolean = nestedInVarLenMapLcl
                }
              }

            potentialBarrierLocationsInAST += (transExpr1 -> newTransition)
            accessSeq1.last.transitions.append(newTransition)
            concatenatedSequences ++ accessSeq1 ++ accessSeq2
        }
      }
    }
  }

  object BuildMemoryAccessGraph {
    def apply(lambda: Lambda): (Option[Access], Predef.Map[Memory, List[Access]],
      Predef.Map[Expr, SynchronizableTransition]) = apply(lambda.body)

    def apply(expr: Expr): (Option[Access], Predef.Map[Memory, List[Access]],
      Predef.Map[Expr, SynchronizableTransition]) =
      new BuildMemoryAccessGraph(expr).apply()
  }

  object ParDomain extends Enumeration {
    type ParDomain = Value
    val Global, Group, Local = Value
  }

  trait Accessor {
    def expr: Expr
    def view: View
  }

  case class Producer(expr: Expr) extends Accessor {
    def view: View = expr.outputView
  }

  case class Consumer(fc: FunCall, arg: Expr) extends Accessor {
    assert(fc.f.isInstanceOf[UserFun])
    assert(fc.args.contains(arg))

    def expr: Expr = arg
    def view: View = arg.view
  }

  /**
   * A temporary case class representing barriers in the memory access graph
   */
  protected case class MutableBarrier(parDomains: mutable.Set[ParDomain]) extends Pattern(arity = 1) {
    override def checkType(argType: Type, setType: Boolean): Type = throw new NotImplementedError()
    override def eval(valueMap: ValueMap, args: Any*): Any = throw new NotImplementedError()
  }
}
