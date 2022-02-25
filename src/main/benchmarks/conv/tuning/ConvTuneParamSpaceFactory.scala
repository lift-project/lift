package benchmarks.conv.tuning

import analysis.ParMapIterations
import analysis.ParMapIterations.ParKind.ParKind
import analysis.ParMapIterations.{ParKind, ParMapDescriptor}
import exploration.constraint_solvers.ChocoConstraintSolver.SyntacticSugar._
import com.typesafe.scalalogging.Logger
import exploration.ParamConstraints.lessThanOrEqual
import exploration.constraint_solvers.ConstraintSolver
import exploration.{ParamConstraint, ParameterRewrite, ParameterSpace}
import exploration.PredicateWrappers._
import ir.ast.Lambda
import lift.arithmetic.simplifier.SimplifyCeiling
import lift.arithmetic.{?, ArithExpr, Cst, NotEvaluableException, SimplifiedExpr, Var}
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir.{CollectTypedOpenCLMemory, InferOpenCLAddressSpace, OpenCLMemoryAllocator, RemoveRedundantMemory}
import org.chocosolver.solver.Model
import benchmarks.conv.layer_configs.LayerConfigFactory
import benchmarks.conv.{ConvExplorationJSONSettings, RunConfig}

import scala.collection.immutable.ListMap
import scala.collection.mutable

trait ConvTuneParamSpaceFactory {
  private val logger = Logger(this.getClass)

  val l: LayerConfigFactory.type = LayerConfigFactory

  def tuneParams: Vector[Var]
  def tuneParamValuesToString(paramValues: Predef.Map[Var, Cst], nWrgs: Vector[Long]): String

  val localSizes: Vector[Var]

  private var _nWrgsLimits: Option[Vector[List[ArithExpr]]] = None

  def nWrgsLimits: Vector[List[ArithExpr]] = _nWrgsLimits match {
    case None => throw new IllegalStateException(s"Cannot retrieve nWrgsLimits because they have not been initialized yet")
    case Some(someNWrgsLimits) => someNWrgsLimits
  }

  def initNWrgsLimits(rewrittenLambda: Lambda): Unit = {
    val inferredParMapIterations = ParMapIterations(rewrittenLambda)

    val limits: mutable.Map[(ParKind, Int), List[ArithExpr]] = mutable.Map()

    inferredParMapIterations.foreach {
      case (ParMapDescriptor(ParKind.Workgroup, dim), iters) =>
        val parKind = (ParKind.Workgroup, dim)

        limits.update(parKind, limits.get(parKind) match {
          case Some(l) => l ++ iters
          case None => iters
        })

      case (ParMapDescriptor(ParKind.Global, dim), iters) =>
        val parKind = (ParKind.Workgroup, dim)

        limits.update(parKind, limits.get(parKind) match {
          case Some(l) => l ++ iters.map(_ /^ localSizes(dim))
          case None => iters.map(_ /^ localSizes(dim))
        })

      case _ =>
    }

    _nWrgsLimits = Some(
      (0 until 3).map(i =>
        limits.get((ParKind.Workgroup, i)) match {
          case Some(limits) => limits
          case None => List(Cst(1))
        }
      ).toVector)
  }

  def nWrgs(allParamValues: ListMap[Var, Cst]): Vector[Long] = {

    (0 until 3).map(i => {

      val allWrgIters = nWrgsLimits(i).map(e =>
        SimplifyCeiling(
          ArithExpr.substitute(e, Map(allParamValues.toSeq: _*))))

      val wrgItersMax = allWrgIters.sortWith {
        case (a, b) => ArithExpr.isSmaller(a, b) match {
          case None => throw new IllegalArgumentException(s"Cannot set the range of nWrgs($i) because isSmaller of " +
            s"$a and $b cannot be determined")
          case Some(is) => is
        }
      }.last

      assert(wrgItersMax.isEvaluable)
      assert(wrgItersMax.evalLong > 0)

      wrgItersMax.evalLong
    }).toVector
  }

  def replaceGenericParamsWithPredefined(f: Lambda): Lambda = {
    val allParams = l.layerConfigParams ++ tuneParams

    ParameterRewrite(f, {
      case v: Var if allParams.exists(_.name.equals(v.name)) =>
        allParams.find(_.name.equals(v.name)).get
    })
  }

  def heuristicConstraints(rewrittenLambda: Lambda)
                          (implicit runConfig: RunConfig,
                           jsonSettings: ConvExplorationJSONSettings): Vector[ParamConstraint] = {

    // NB: Might prevent generation of the best point for L8 direct. TBC
    val heuLocalSizeLeIterations = {
      val inferredParMapIterations = ParMapIterations(rewrittenLambda)

      val limits: mutable.Map[(ParKind, Int), List[ArithExpr]] = mutable.Map()

      inferredParMapIterations.foreach {
        case (ParMapDescriptor(ParKind.Local | ParKind.Global, dim), iters) =>
          val parKind = (ParKind.Local, dim)

          limits.update(parKind, limits.get(parKind) match {
            case Some(l) => l ++ iters
            case None => iters
          })
        case _ =>
      }

      limits.flatMap { case ((_, dim), iterations) =>
        //    val tuneParam = mapKind match {
        //      case ParKind.Local      => /*println(f"$mapKind $dim $iterations"); */localSizes(dim)
        ////        case ParKind.Workgroup  => nWrgs(dim)
        ////          TODO: use nWrgsLimits to check total number of threads
        //      case ParKind.Global     => localSizes(dim) * nWrgs(dim)
        //      case _ => throw new IllegalArgumentException
        //    }

        Vector(
          ParamConstraint(
            s"heu.localSize$dim.le.iterations",
            s"Heuristic: the number of local threads must be less or equal to the maximum " +
              s"number of iterations in the corresponding local or global dimension (max(" + iterations + "))",
            params = List(localSizes(dim)) ++ iterations,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
              try {
                val maxIterations = args.tail.foldLeft[ArithExpr](Cst(1))(ArithExpr.max)
                //            println(tuneParam.toString + " maxIterations = " + maxIterations.toString)

                lessThanOrEqual(args.head, maxIterations)
              } catch {
                case e: NotEvaluableException =>
                  args.tail.map(lessThanOrEqual(args.head, _)).reduce(_ || _)
                case e => throw e
              }
            },

            predicate = (model: Model, params: List[ArithExpr]) => {
              implicit val (m, p) = (model, params.toArray)

              (try {
                val maxIterations = params.tail.foldLeft[ArithExpr](1)(ArithExpr.max).evalLong

                params.head <= toChoco(maxIterations)
              } catch {
                case e: NotEvaluableException =>
                  or(params.tail.map(oneOfTheMaximums => params.head <= oneOfTheMaximums): _*)
                case e => throw e
              }).post()
            })
        )
      }.toVector
    }
//    heuLocalSizeLeIterations
    Vector()
  }

  def hardwareConstraints(f: Lambda,
                          layerConfigValues: ListMap[Var, Cst]
                         )(implicit jsonSettings: ConvExplorationJSONSettings): Vector[ParamConstraint] = {
    //    View(f)

    // Disabled since this is already enforced by localSize var range
    //    val localSizesMustComplyToHWSpec =
    //      (0 until 3).toVector.zip(
    //        Vector(jsonSettings.maxLocalSize0,
    //          jsonSettings.maxLocalSize1,
    //          jsonSettings.maxLocalSize2))
    //        .map { case (dim: Int, maxLocalSize: Int) =>
    //
    //          ParamConstraint(
    //            f"localSize${dim}MustBeSmallerThanMaxLocalSize",
    //            f"Local size in dimension $dim must not be greater than max local " +
    //              f"size ($maxLocalSize)",
    //            params = List(localSizes(dim), maxLocalSize),
    //            predicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
    //            chocoPredicateAsStr = "<=")
    //        }

    val wrgSizesMustComplyToHWSpec = {
      val workGroupSize = localSizes.foldLeft[ArithExpr](Cst(1))(_ * _)

      ParamConstraint(
        s"wrgSizeMustBeSmallerThanMaxWrgSize",
        f"Workgroup size must not be greater than max work group " +
          f"size (${jsonSettings.maxWorkgroupSize})",
        params = List(workGroupSize, jsonSettings.maxWorkgroupSize),
        arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
        predicate = "<=")
    }

      // TODO: reenable until Z3 is integrated (this and/or the private mem constraint caused integer overflow in Choco)
    val maxLocalMemConsumption = {

      val localIntermediateMemories = CollectTypedOpenCLMemory(f, includePrivate = true)._4
      if (localIntermediateMemories.nonEmpty) {
        val totalLocalMemConsum = localIntermediateMemories.map(_.mem.size).foldLeft[ArithExpr](Cst(0))(_ + _)

        if (totalLocalMemConsum == Cst(0))
          Vector()
        else Vector(ParamConstraint(
          s"totalLocalMemConsumMemMustBeNoGreaterThanLocalMemSize",
          f"Total local memory consumption must be no greater than local memory size (${jsonSettings.localMemSize})",
          params = List(totalLocalMemConsum, jsonSettings.localMemSize),
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
          predicate = "<="))
        //          This is safe since we check memory consumption during compilation
        //          mightOverflow = true))
      } else Vector()
    }

    // Way too restrictive since it needs to be applied only on MapLcls with barriers, and those are not known before BarrierInsertion can be run on a concrete lambda.
    /*val synchronizability = {

      val mapLcls = Expr.visitWithState(Set[MapLcl]())(f.body, {
        case (FunCall(m: MapLcl, _*), collectedMapLcls) => collectedMapLcls + m
        case (_, collectedMapLcls) => collectedMapLcls
      })

      mapLcls.map(mapLcl => {
        assert(mapLcl.loopVar.range.isInstanceOf[RangeAdd])
        val loopRange = mapLcl.loopVar.range.asInstanceOf[RangeAdd]
// only for mapLcls with barriers!
        ParamConstraint(s"mapLclRangeSizeMustBeMultipleOfStep",
          f"MapLcl range size must be a multiple of step, i.e. all threads must perform the same number of iterations",
          // No need to subtract start from stop because we know that the loop starts with get_local_id, which we cannot translate into a constraint
          params = List(loopRange.stop % loopRange.step, Cst(0)),
          predicate = (args: List[ArithExpr with SimplifiedExpr]) => args.head == args(1),
          chocoPredicateAsStr = "==")
      }).toVector
      // gen cstr that step must be a factor of UB
    }*/
    // For info about registers in Mali G71 (same as G72), see:
    // https://community.arm.com/developer/tools-software/graphics/b/blog/posts/the-mali-gpu-an-abstract-machine-part-4---the-bifrost-shader-core
    // Confirmation of G72's 64 registers:
    // https://www.anandtech.com/show/12834/arm-announces-the-mali-g76-scaling-up-bifrost/2

    /*val memories = CollectTypedOpenCLMemory(rewrittenLambda, includePrivate = true)

    val privateMemories = {
      val (_, lp) = (memories._1 ++ memories._2 ++ memories._3 ++ memories._4).
        partition(_.mem.addressSpace == GlobalMemory)
      val (_, p) = lp.partition(_.mem.addressSpace == LocalMemory)
      p
    }

    val totalSizeOfPrivate = privateMemories.map(_.mem.size).foldLeft[ArithExpr](Cst(0))(_ + _)
    val maxPrivateFloatsAllowed = jsonSettings.floatRegistersPerThreadInMaxOccupancy * 4 * 4

    if (totalSizeOfPrivate == Cst(0))
      Vector()
    else Vector(ParamConstraint(
      s"heu.totalSizeOfPrivateMustBeNoGreaterThan$maxPrivateFloatsAllowed",
      s"Heuristic: The total size of the private memory must equal $maxPrivateFloatsAllowed (the total size of available " +
        s"registers multiplied by four to account for variable elimination)",
      params = List(totalSizeOfPrivate, Cst(maxPrivateFloatsAllowed)),
      arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
      predicate = "<="
    ))*/

    /*localSizesMustComplyToHWSpec ++ */ Vector(wrgSizesMustComplyToHWSpec)// ++ maxLocalMemConsumption //++ synchronizability
  }

  def extraRangeConstraints: Vector[ParamConstraint] = Vector()

  def debuggingConstraints(layerConfigIdx: Int)(implicit runConfig: RunConfig): Vector[ParamConstraint]


  /**
   * Creates tune param space with given layer config values, rewrite params and inferred constraints
   */
  def getTuneParamSpace(layerConfigIdx: Int,
                        layerConfigValues: ListMap[Var, Cst],
                        rewrittenLambda: Lambda,
                        inferredConstraints: Vector[ParamConstraint],
                        initialSeed: Int
                       )(implicit runConfig: RunConfig, jsonSettings: ConvExplorationJSONSettings): ParameterSpace = {

    initNWrgsLimits(rewrittenLambda)

    // Needed by both heuristic and hardware constraints
    InferOpenCLAddressSpace(rewrittenLambda)
    RangesAndCounts(rewrittenLambda,
      NDRange(localSizes(0), localSizes(1), localSizes(2)),
      NDRange(?, ?, ?), Map(layerConfigValues.toSeq: _*))
    OpenCLMemoryAllocator(rewrittenLambda)
    RemoveRedundantMemory(rewrittenLambda)

    val heuConstraints = heuristicConstraints(rewrittenLambda)
    val hwConstraints = hardwareConstraints(rewrittenLambda, layerConfigValues)
    val debugConstraints = debuggingConstraints(layerConfigIdx)

    val searchSpace = new ParameterSpace(
      independentParameters = layerConfigValues,
      dependentParameters = tuneParams,
      inferredAndManualConstraints =
        inferredConstraints ++ extraRangeConstraints ++ heuConstraints ++ hwConstraints ++ debugConstraints)

    searchSpace.setSolver(ConstraintSolver.Choco, initialSeed)
    //    searchSpace.setSolver(ConstraintSolver.Z3, initialSeed)

    logger.info("Initialised conv tune param space. Here are some stats:\n" +
      s"Heuristic constraints number: ${heuConstraints.length}\n" +
      s"Hardware constraints number: ${hwConstraints.length}\n" +
      s"Inferred constraints number: ${inferredConstraints.length}\n" +
      s"Debugging constraints number: ${debugConstraints}\n" +
      s"Heuristic constraints:\n" + heuConstraints.map(_.name).mkString("\n") + "\n" +
      s"Debugging constraints:\n" + debugConstraints.map(c => c.name + ": " + c.comment).mkString("\n"))

    searchSpace
  }
}
