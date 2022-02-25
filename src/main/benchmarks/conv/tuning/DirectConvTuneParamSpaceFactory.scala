package benchmarks.conv.tuning

import benchmarks.conv.{ConvExplorationJSONSettings, RunConfig}
import exploration.ParamConstraint
import exploration.ParamConstraints.greaterThanOrEqual
import exploration.PredicateWrappers._
import ir.ast.Lambda
import lift.arithmetic._
import benchmarks.conv.ConvExploration.debugRun

import scala.collection.immutable.ListMap

case class DirectConvTuneParamSpaceFactory()(implicit val jsonSettings: ConvExplorationJSONSettings)
  extends ConvTuneParamSpaceFactory {

  /** Tune params */
  // Division by 2 is an arbitrary bound to work around nWrg upper bound integer overflow in Choco
  val padOptRight: Var = Var("padOptRight", RangeAdd(0, (l.inputWidth + l.padFuncX * 2) / 2, step = 1))
  val padOptBottom: Var = Var("padOptBottom", RangeAdd(0, (l.inputHeight + l.padFuncY * 2) / 2, step = 1))

  /** Derivative formulas */
  val inputWidthPadded: ArithExpr = l.inputWidth + l.padFuncX * 2 + padOptRight
  val inputHeightPadded: ArithExpr = l.inputHeight + l.padFuncY * 2 + padOptBottom
  val nWindowsAcrossInputInXOptPadded: ArithExpr = (inputWidthPadded - (l.kernelWidth - l.kernelStrideX)) / l.kernelStrideX
  val nWindowsAcrossInputInYOptPadded: ArithExpr = (inputHeightPadded - (l.kernelHeight - l.kernelStrideY)) / l.kernelStrideY

  /** More tune params */
  val tileWidth: Var = Var("tileWidth", RangeAdd(1, nWindowsAcrossInputInXOptPadded + 1, step = 1)) // in (partial) windows
  val tileHeight: Var = Var("tileHeight", RangeAdd(1, nWindowsAcrossInputInYOptPadded + 1, step = 1)) // in (partial) windows
  val tileDepth: Var = Var("tileDepth", RangeAdd(1, l.inputChannels + 1, step = 1)) // in channels
  val kernelGroupSize: Var = Var("kernelGroupSize", RangeAdd(1, l.numKernels + 1, step = 1))

  val seqWindowsPerThreadX: Var = Var("seqWindowsPerThreadX", RangeAdd(1, tileWidth + 1, step = 1)) // in (partial) windows
  val seqWindowsPerThreadY: Var = Var("seqWindowsPerThreadY", RangeAdd(1, tileHeight + 1, step = 1)) // in (partial) windows
  val seqKernelsPerThread: Var = Var("seqKernelsPerThread", RangeAdd(1, kernelGroupSize + 1, step = 1)) // in (partial) windows

  val windowTileWidth: Var = Var("windowTileWidth", RangeAdd(1, l.kernelWidth + 1, step = 1)) // in elements
  val windowTileHeight: Var = Var("windowTileHeight", RangeAdd(1, l.kernelHeight + 1, step = 1)) // in elements

  val inputCacheSizeX: Var = Var("inputCacheSizeX", RangeAdd(1, seqWindowsPerThreadX + 1, step = 1)) // in (partial) windows
  val inputCacheSizeY: Var = Var("inputCacheSizeY", RangeAdd(1, seqWindowsPerThreadY + 1, step = 1)) // in (partial) windows
  val inputCacheDepth: Var = Var("inputCacheDepth", RangeAdd(1, tileDepth + 1, step = 1)) // in channels
  val kernelCacheSize: Var = Var("kernelCacheSize", RangeAdd(1, seqKernelsPerThread + 1, step = 1))

  val localSizes: Vector[Var] =
    (0 until 3).map(i => Var(s"localSize$i", RangeAdd(1, jsonSettings.maxLocalSize(i), step = 1))).toVector

  def tuneParams: Vector[Var] = Vector(
    padOptRight, padOptBottom,
    tileWidth, tileHeight, tileDepth, kernelGroupSize,
    seqWindowsPerThreadX, seqWindowsPerThreadY, seqKernelsPerThread,
    windowTileWidth, windowTileHeight,
    inputCacheSizeX, inputCacheSizeY, inputCacheDepth, kernelCacheSize) ++ localSizes

  /* Nicely formatted parameter value string for debugging */
  def tuneParamValuesToString(paramValues: Predef.Map[Var, Cst], nWrgs: Vector[Long]): String =
    s"""padOptRight -> ${paramValues(padOptRight).c}, padOptBottom -> ${paramValues(padOptBottom).c},
       |tileWidth -> ${paramValues(tileWidth).c}, tileHeight -> ${paramValues(tileHeight).c}, tileDepth -> ${paramValues(tileDepth).c},
       |seqWindowsPerThreadX -> ${paramValues(seqWindowsPerThreadX).c}, seqWindowsPerThreadY -> ${paramValues(seqWindowsPerThreadY).c},
       |seqKernelsPerThread -> ${paramValues(seqKernelsPerThread).c},
       |kernelGroupSize -> ${paramValues(kernelGroupSize).c},
       |windowTileWidth -> ${paramValues(windowTileWidth).c}, windowTileHeight -> ${paramValues(windowTileHeight).c},
       |inputCacheSizeX -> ${paramValues(inputCacheSizeX).c}, inputCacheSizeY -> ${paramValues(inputCacheSizeY).c},
       |inputCacheDepth -> ${paramValues(inputCacheDepth).c},
       |kernelCacheSize -> ${paramValues(kernelCacheSize).c}""".stripMargin +
      (0 until 3).map(i => s"localSizes($i) -> ${paramValues(localSizes(i)).c}").mkString(",\n", ",\n", ",\n") +
      (0 until 3).map(i => s"nWrgs($i) -> ${nWrgs(i)}").mkString("", ",\n", "\n")

  /* Derivative formulas */
  //  val nWindowsAcrossTileInX: ArithExpr = (tileWidth - (kernelWidth - kernelStrideX)) / kernelStrideX
  //  val nWindowsAcrossTileInY: ArithExpr = (tileHeight - (kernelHeight - kernelStrideY)) / kernelStrideY
  //  val nKernelsPerThread: ArithExpr = numKernels / kernelCacheSize

  //  val partialResultsTheoreticalMaxSize: ArithExpr = numKernels * outputWidth * outputHeight * (inputChannels / tileDepth)

  ////  val totalKernelCacheSizeInFloats: ArithExpr = kernelCacheSize * kernelWidth * kernelHeight * tileDepth
  ////val totalKernelCacheSizeInFloats: ArithExpr = kernelCacheSize * kernelWidth * kernelHeight * inputCacheDepth
  //  val totalKernelCacheSizeInFloats: ArithExpr = kernelCacheSize * windowTileWidth * windowTileHeight * inputCacheDepth
  //  //  val totalInputCacheSizeInFloats: ArithExpr = inputCacheSizeY * inputCacheSizeX * kernelWidth * kernelHeight * tileDepth
  ////  val inputCacheCoverageOverInputX: ArithExpr = (kernelWidth - kernelStrideX) + kernelStrideX * inputCacheSizeX // unslided formula that doesn't work with partial windows
  ////  val inputCacheSizeInUniqueFloats: ArithExpr = inputCacheCoverageOverInputY * inputCacheCoverageOverInputX  * tileDepth
  //  val inputCacheSizeInUniqueFloats: ArithExpr = totalInputCacheHeightInFloats * totalInputCacheWidthInFloats  * inputCacheDepth


  override def heuristicConstraints(rewrittenLambda: Lambda)
                                   (implicit runConfig: RunConfig,
                                    jsonSettings: ConvExplorationJSONSettings): Vector[ParamConstraint] =
    super.heuristicConstraints(rewrittenLambda) ++
      /*Vector(new ParamConstraint(
        "partialResultsBufferMaxSizeMustBeLessThan",
        "The buffer max size containing partial results must be less than or equal to 1 MB (to be reduced empirically)",
        params = List(
          partialResultsTheoreticalMaxSize,
          Cst(1073741824) / Cst(4) /* float size */),
        predicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
        predicateAsStr = "<="
      )) ++ */ {
      /** Deprecating this in favour of a more robust BenchmarkStages.validateMemorySizes() * */

      // For info about registers in Mali G71 (same as G72), see:
      // https://community.arm.com/developer/tools-software/graphics/b/blog/posts/the-mali-gpu-an-abstract-machine-part-4---the-bifrost-shader-core
      // Confirmation of G72's 64 registers:
      // https://www.anandtech.com/show/12834/arm-announces-the-mali-g76-scaling-up-bifrost/2

      //    val privateCacheSize =
      //      if (debugRun) {
      //        Cst(0)
      //      } else {
      //        val prefetchingParams = CollectPrefetching(rewriteParamSpace.rewriteStrategies).rewriteParams
      //        val prefetchingParams.RewriteParamValues6(inputCacheAddressSpace, kernelCacheAddressSpace, _, _, _, _) =
      //          rewriteParamValues(prefetchingParams)
      //
      //        (if (inputCacheAddressSpace.contains(PrivateMemory)) inputCacheSizeInUniqueFloats else Cst(0)) +
      //          (if (kernelCacheAddressSpace.contains(PrivateMemory)) totalKernelCacheSizeInFloats else Cst(0))
      //      }
      //
      //    if (privateCacheSize == Cst(0)) Vector()
      //    else Vector(ParamConstraint(
      //      "heu.PrivateCacheSizeMustBeLessThan",
      //      s"Heuristic: The total size of input and kernel caches in private memory -- taken as a total number of " +
      //        s"unique data elements in the caches (the OpenCL compiler removes duplicate reads) --" +
      //        s"must be less than or equal to ${jsonSettings.floatRegistersPerThreadInMaxOccupancy} for maximum occupancy",
      //      params = List(privateCacheSize, Cst(jsonSettings.floatRegistersPerThreadInMaxOccupancy)),
      //      predicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
      //      chocoPredicateAsStr = "<="
      //    ))
      Vector()
    } ++ {
      // This Choco constraint is valid for minParThreads in [0..pow(2^31, 1/3)] = [0..1290]
      // due Choco's integer overflow checks for multiplication
      val minParThreads = 12
      assert(Math.pow(minParThreads, 6) < Math.pow(2, 31)) // Preventing the overflow error in Choco. Approx max: 35
      Vector(
        // TODO: make it work with new nWrgs that are not tune params
        //      ParamConstraint(
        //        s"heu.nThreads>$minParThreads",
        //        s"Heuristic: total number of threads should be more than $minParThreads",
        //        // TODO: use nWrgsLimits to check total number of threads
        //        params = (localSizes ++ nWrg).toList,
        //        predicate = (args: List[ArithExpr with SimplifiedExpr]) =>
        //          greaterThanOrEqual(args.take(3).zip(args.drop(3))
        //            .map(p => p._1 * p._2)
        //            .reduce(_ * _), minParThreads),
        //
        //        chocoPredicateAsFun = (model: Model, params: List[ArithExpr]) => {
        //          implicit val (m, p) = (model, params.toArray)
        //          val ls = params.take(3)
        //          val ws = params.drop(3)
        ////          val prec = 0.001d // precision
        //
        //          // These conditions are all to solve the problem of multiplying three variables (nWrgs) with
        //          // the upper bound of Integer.MAX_Value - 1, which would trigger an error from Choco
        //          (or(ls.map(_ >= minParThreads): _*) or
        //            (ls.map(toChoco(_)).reduce(_ * _) >= minParThreads) or
        //            or(ws.map(_ >= minParThreads): _*) or
        //            // At this point we now that all w and l are < minParThreads, so we can cap them for safe multiplication (no overflow):
        //            // E.g. for minParThreads = 12, min(12, w0)*min(12, w1)*min(12, w0) * min(12, l0)*min(12, l1)*min(12, l0) >= 12
        //            // minParThreads still can't be too high, since the expression below has an upper bound of minParThreads^6
        //            ws.map(toChoco(_).min(minParThreads)).reduce(_ * _) *
        //              ls.map(toChoco(_).min(minParThreads)).reduce(_ * _) >= toChoco(minParThreads)
        //                        // Convert to double for larger range (2^1023) instead of Int's 2^31
        ////            (ls.zip(ws).map(p => realIntView(toChoco(p._1), prec) * realIntView(toChoco(p._2), prec))
        ////              .reduce(_ + _) >= realIntView(12, prec)
        ////              ).ibex(prec)
        //            ).post()
        //        }
        //      )
      )
    } ++ {
      val wrgSizeMustBeLargerThan = {
        val workGroupSize = localSizes.foldLeft[ArithExpr](Cst(1))(_ * _)

        val minWrgSize = 48
        // TODO: replace with minParThreads
        ParamConstraint(
          s"wrgSizeMustBeLargerOrEqualTo$minWrgSize",
          f"Workgroup size must be larger than or equal to max $minWrgSize",
          params = List(workGroupSize, Cst(minWrgSize)),
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => greaterThanOrEqual(args.head, args(1)),
          predicate = ">=")
      }
      if (runConfig.chooseLocalSizeDeterministically)
        Vector(wrgSizeMustBeLargerThan)
      else Vector()
//      Vector()
    } ++ {
      /* // Disabling because it prevents from finding good points
      Vector(ParamConstraint("heu.wrgSize.mod.4.eq.zero",
        "Heuristic: the workgroup size should be a multiple of 4",
        params = List((localSizes(0) * localSizes(1) * localSizes(2)) % 4, Cst(0)),
        predicate = (args: List[ArithExpr with SimplifiedExpr]) => args.head == args(1),
        chocoPredicateAsStr = "=="))
    }*//* ++ { // This constraint sets the number of threads exactly to the number of iterations one of the corresponding maps performs
      val inferredParMapIterations = ParMapIterations(rewrittenLambda)
      val allParMapKinds =
        (0 until 3).map(ParMapDescriptor(ParKind.Workgroup, _)) ++
          (0 until 3).map(ParMapDescriptor(ParKind.Local, _))
      val allParMapIterations = allParMapKinds.map(mapKind => mapKind ->
        inferredParMapIterations.getOrElse(mapKind, List(Cst(1)))).toMap

      allParMapIterations.flatMap { case (ParMapDescriptor(mapKind, dim), iterations) =>
        val tuneParam = mapKind match {
          case ParKind.Local      => localSizes(dim)
          case ParKind.Workgroup  => nWrgs(dim)
          case _ => throw new IllegalArgumentException
        }

        Vector(
          ParamConstraint(
            s"heu.${tuneParam.name}==(processed els in one of the instances)==${iterations.last}",
            s"Heuristic: ${tuneParam.name} must be equal to the number of elements that one (last in depth-first order) " +
              s"of the instances of corresponding maps (MapLcl or MapWrg) processes:\n" + iterations.last,
            params = List(tuneParam, iterations.last),
            predicate = (args: List[ArithExpr with SimplifiedExpr]) => args.head == args(1),
            chocoPredicateAsStr = "=="
          )
        )}.toVector
    */ Vector()
    } ++ {
      Vector(
//        ParamConstraint(
//          s"heu.padoptRight.le.20percent.of.inputWidthPadded",
//          s"Heuristic: padOptRight <= inputWidthPadded / 5",
//          params = List(padOptRight, inputWidthPadded / 5),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
//          predicate = "<="),
//
//        ParamConstraint(
//          s"heu.padoptBottom.le.20percent.of.inputHeightPadded",
//          s"Heuristic: padOptBottom <= inputHeightPadded / 5",
//          params = List(padOptBottom, inputHeightPadded / 5),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => lessThanOrEqual(args.head, args(1)),
//          predicate = "<=")//,


//        ParamConstraint(
//          s"heu.kernelTileSize.lt.589824",
//          s"Heuristic: kernelTileSize < 589824",
//          params = List(kernelGroupSize * tileDepth * l.kernelHeight * l.kernelWidth, 589824),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => ArithExpr.isSmaller(args.head, args(1)).get,
//          predicate = "<"),
//
//        ParamConstraint(
//          s"heu.seqInputBatchSize.lt.23000",
//          s"Heuristic: seqInputBatchSize < 23000",
//          params = List(seqWindowsPerThreadY * seqWindowsPerThreadX * l.kernelHeight * l.kernelWidth * tileDepth, 23000),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => ArithExpr.isSmaller(args.head, args(1)).get,
//          predicate = "<"),
//
//        ParamConstraint(
//          s"heu.seqKernelBatchSize.lt.9216",
//          s"Heuristic: seqKernelBatchSize < 9216",
//          params = List(seqKernelsPerThread * l.kernelHeight * l.kernelWidth * tileDepth, 9216),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => ArithExpr.isSmaller(args.head, args(1)).get,
//          predicate = "<"),
//
//        ParamConstraint(
//          s"heu.totalInputCacheSize.ge.4",
//          s"Heuristic: totalInputCacheSize >= 4",
//          params = List(inputCacheSizeY * inputCacheSizeX * inputCacheDepth * windowTileHeight * windowTileWidth, 4),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => greaterThanOrEqual(args.head, args(1)),
//          predicate = ">="),
//
//        ParamConstraint(
//          s"heu.totalKernelCacheSize.ge.4",
//          s"Heuristic: totalKernelCacheSize >= 4",
//          params = List(kernelCacheSize * inputCacheDepth * windowTileHeight * windowTileWidth, 4),
//          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => greaterThanOrEqual(args.head, args(1)),
//          predicate = ">=")

      )
    }

  override def debuggingConstraints(layerConfigIdx: Int)(implicit runConfig: RunConfig): Vector[ParamConstraint] = {
    val bestTuneParams = ListMap[Int, (ListMap[Var, Int], ListMap[Var, Int])](
      0 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 16,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 3,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 3,
        kernelCacheSize -> 4), ListMap(
        localSizes(0) -> 1, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16)),
      1 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 16,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      )),
      2 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 16,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))
      )),
      3 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 16,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      )),
      4 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 8,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))
      )),
      7 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 4,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))
      )),
      5 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 8,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8, // tileDepth / inputCacheDepth
        localSizes(1) -> 3,
        localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))
      )),
      8 -> (ListMap(
        padOptRight -> 4, padOptBottom -> 4,
        tileWidth -> 4, tileHeight -> 16, tileDepth -> 32, //128,
        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 8,
        localSizes(1) -> 3,
        localSizes(2) -> 16
      )),
      10 -> (ListMap(
        padOptRight -> 1, padOptBottom -> 1,
        tileWidth -> 5, tileHeight -> 5, tileDepth -> 16,//128,
        seqWindowsPerThreadX -> 5, seqWindowsPerThreadY -> 1,
        seqKernelsPerThread -> 4,
        kernelGroupSize -> 4,
        windowTileWidth -> 1, windowTileHeight -> 1,
        inputCacheSizeX -> 5, inputCacheSizeY -> 1,
        inputCacheDepth -> 4,
        kernelCacheSize -> 4
      ), ListMap(
        localSizes(0) -> 4,
        localSizes(1) -> 3,
        localSizes(2) -> 5
      )))
//    val manualTuneParams = collection.immutable.ListMap[Var, Int](
      // layer 10
      //      tileWidth -> 1, tileHeight -> 1, tileDepth -> 1,
      //      kernelGroupSize -> 512,
      //      inputCacheSizeX -> 1, inputCacheSizeY -> 1,
      //      kernelCacheSize -> 2,
      //      localSizes(0) -> 1,
      //      localSizes(1) -> 1,
      //      localSizes(2) -> 1,
      //      nWrgs(0) -> 512,
      //      nWrgs(1) -> 1,
      //      nWrgs(2) -> 1
      //      tileWidth -> 3, tileHeight -> 3, tileDepth -> 32,
      //      kernelGroupSize -> 1,
      //      inputCacheSizeX -> 3, inputCacheSizeY -> 3,
      //      kernelCacheSize -> 1,
      //      localSizes(0) -> 16,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 1
      //      nWrgs(0) -> 76,
      //      nWrgs(1) -> 38,
      //      nWrgs(2) -> 128
      // layer 2, Lu's choices
      //      padOptRight -> 2, padOptBottom -> 2,
      //      tileWidth -> 3, tileHeight -> 3, tileDepth -> 32,
      //      seqWindowsPerThreadX -> 3, seqWindowsPerThreadY -> 3,
      //      seqKernelsPerThread -> 1,
      //      kernelGroupSize -> 1,
      //      windowTileWidth -> 3, windowTileHeight -> 3,
      //      inputCacheSizeX -> 3, inputCacheSizeY -> 3,
      //      inputCacheDepth -> 1,
      //      kernelCacheSize -> 1,
      //      localSizes(0) -> 1,
      //      localSizes(1) -> 1,
      //      localSizes(2) -> 1,
      //      nWrgs(0) -> 38*38,
      //      nWrgs(1) -> 2,high
      //      nWrgs(2) -> 128

      // Our old best for old layout (kernel_sharing_coalesce_fix) L11: 16 ms
      //      padOptRight -> 1, padOptBottom -> 1,
      //      tileWidth -> 5, tileHeight -> 5, tileDepth -> 16,//128,
      //      seqWindowsPerThreadX -> 5, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 5, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 4,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 5

      // Our old best for old layout (kernel_sharing_coalesce_fix) L8: 103 ms
      //      padOptRight -> 2, padOptBottom -> 2,
      //      tileWidth -> 3, tileHeight -> 3, tileDepth -> 16,//128,
      //      seqWindowsPerThreadX -> 3, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 3, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 4,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 3,
      //      nWrgs(0) -> 128,
      //      nWrgs(1) -> 100,
      //      nWrgs(2) -> 1
      // Our old best for old layout (kernel_sharing_coalesce_fix) L8: 91 ms
      //      padOptRight -> 2, padOptBottom -> 2,
      //      tileWidth -> 3, tileHeight -> 3, tileDepth -> 32,//128,
      //      seqWindowsPerThreadX -> 3, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 3, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 3,
      //      nWrgs(0) -> 128,
      //      nWrgs(1) -> 100,
      //      nWrgs(2) -> 1
      // Our old best for old layout (kernel_sharing_coalesce_fix) L8: 76.74
      //      padOptRight -> 2, padOptBottom -> 2,
      //      tileWidth -> 6, tileHeight -> 6, tileDepth -> 32,//128,
      //      seqWindowsPerThreadX -> 3, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 3, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 6,
      //      nWrgs(0) -> 128,
      //      nWrgs(1) -> 25,
      //      nWrgs(2) -> 1
      // Our old best for old layout (kernel_sharing_coalesce_fix) L8: 71.14
      //      padOptRight -> 4, padOptBottom -> 4,
      //      tileWidth -> 8, tileHeight -> 8, tileDepth -> 32,//128,
      //      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 8,
      //      nWrgs(0) -> 128,
      //      nWrgs(1) -> 16,
      //      nWrgs(2) -> 1
//    ) ++ (if (layerConfigIdx == 8) ListMap(
//      // Our old best for old layout (kernel_sharing_coalesce_fix) L8: 58.14 ms
//      padOptRight -> 4, padOptBottom -> 4,
//      tileWidth -> 4, tileHeight -> 16, tileDepth -> 32, //128,
//      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
//      seqKernelsPerThread -> 4,
//      kernelGroupSize -> 4,
//      windowTileWidth -> 1, windowTileHeight -> 1,
//      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
//      inputCacheDepth -> 4,
//      kernelCacheSize -> 4) ++
//      (if (runConfig.chooseLocalSizeDeterministically)
//        ListMap[Var, Int]()
//      else
//        ListMap[Var, Int](
//          localSizes(0) -> 8,
//          localSizes(1) -> 3,
//          localSizes(2) -> 16)) else ListMap()) ++
//      collection.immutable.ListMap[Var, Int](

      // Our old best for old layout (kernel_sharing_coalesce_fix) L8: barrier insertion bug during exploration (solved)
      //      padOptRight -> 14, padOptBottom -> 13,
      //      tileWidth -> 1, tileHeight -> 41, tileDepth -> 4,//128,
      //      seqWindowsPerThreadX -> 1, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 1,
      //      kernelGroupSize -> 1,
      //      windowTileWidth -> 3, windowTileHeight -> 1,
      //      inputCacheSizeX -> 1, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 1,
      //      kernelCacheSize -> 1,
      //      localSizes(0) -> 4,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 30

      // Our old best for old layout (kernel_sharing_coalesce_fix) L4: 32 ms
      //      padOptRight -> 4, padOptBottom -> 8,
      //      tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L4: 61 ms
      //      padOptRight -> 7, padOptBottom -> 7, // wh = 56+2+7= 65
      //      tileWidth -> 3, tileHeight -> 3, tileDepth -> 8, // (65-2)/3=21
      //      seqWindowsPerThreadX -> 3, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 1,/**/
      //      kernelGroupSize -> 1,/**/
      //      windowTileWidth -> 3, windowTileHeight -> 1,
      //      inputCacheSizeX -> 3, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 1,/**/
      //      localSizes(0) -> 8,
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 3 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L4: 162 ms
      //      padOptRight -> 4, padOptBottom -> 8,  // 56+2+8=66
      //      tileWidth -> 4, tileHeight -> 16, tileDepth -> 64, // (iW+2+padRight - 2) % tileWidth
      //      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 8,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 8,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L4: 47 ms
      //      padOptRight -> 4, padOptBottom -> 8,
      //      tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //      seqWindowsPerThreadX -> 2, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 2,
      //      kernelGroupSize -> 2,
      //      windowTileWidth -> 3, windowTileHeight -> 1,
      //      inputCacheSizeX -> 2, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 2,
      //      localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //      localSizes(1) -> 3, // kernelHeight / windowTileHeight
      //      localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L1: 75.11 ms
      //      padOptRight -> 4, padOptBottom -> 16,
      //      tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L0: 9 ms
      //        padOptRight -> 4, padOptBottom -> 16,
      //    tileWidth -> 4, tileHeight -> 16, tileDepth -> 3,
      //    seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //    seqKernelsPerThread -> 4,
      //    kernelGroupSize -> 4,
      //    windowTileWidth -> 1, windowTileHeight -> 1,
      //    inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //    inputCacheDepth -> 3,
      //    kernelCacheSize -> 4,
      //    localSizes(0) -> 1, // tileDepth / inputCacheDepth
      //    localSizes(1) -> 3,
      //    localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L2: 40.47 ms
      //      padOptRight -> 4, padOptBottom -> 16,
      //      tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))


      // Our old best for old layout (kernel_sharing_coalesce_fix) L3: 68.62 ms
      //      padOptRight -> 4, padOptBottom -> 16,
      //      tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 4,
      //      kernelGroupSize -> 4,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 4,
      //      kernelCacheSize -> 4,
      //      localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //      localSizes(1) -> 3,
      //      localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))


      // Our old best for old layout (kernel_sharing_coalesce_fix) L5: 55.67
      //        padOptRight -> 4, padOptBottom -> 8,
      //    tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //    seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //    seqKernelsPerThread -> 4,
      //    kernelGroupSize -> 4,
      //    windowTileWidth -> 1, windowTileHeight -> 1,
      //    inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //    inputCacheDepth -> 4,
      //    kernelCacheSize -> 4,
      //    localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //    localSizes(1) -> 3,
      //    localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      // Our old best for old layout (kernel_sharing_coalesce_fix) L7: 34.63
      //      padOptRight -> 4, padOptBottom -> 4,
      //    tileWidth -> 4, tileHeight -> 16, tileDepth -> 32,
      //    seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
      //    seqKernelsPerThread -> 4,
      //    kernelGroupSize -> 4,
      //    windowTileWidth -> 1, windowTileHeight -> 1,
      //    inputCacheSizeX -> 4, inputCacheSizeY -> 1,
      //    inputCacheDepth -> 4,
      //    kernelCacheSize -> 4,
      //    localSizes(0) -> 8, // tileDepth / inputCacheDepth
      //    localSizes(1) -> 3,
      //    localSizes(2) -> 16 // l2 must be divisible by (v_tileHeight_17*(1/^(v_seqWindowsPerThreadY_21)))

      //      padOptRight -> 14, padOptBottom -> 1,
      //      tileWidth -> 6, tileHeight -> 1, tileDepth -> 128,
      //      seqWindowsPerThreadX -> 3, seqWindowsPerThreadY -> 1,
      //      seqKernelsPerThread -> 512,
      //      kernelGroupSize -> 512,
      //      windowTileWidth -> 1, windowTileHeight -> 1,
      //      inputCacheSizeX -> 1, inputCacheSizeY -> 1,
      //      inputCacheDepth -> 2, // paper: windowTileDepth
      //      kernelCacheSize -> 128,
      //      localSizes(0) -> 1,
      //      localSizes(1) -> 1,
      //      localSizes(2) -> 356

      // Our old best improved for direct->GEMM + old layout (kernel_sharing_coalesce_fix) on GEMM L8: 72 ms
//      padOptRight -> 0, padOptBottom -> 0,
//      tileWidth -> 4, tileHeight -> 7, tileDepth -> 32, //128,
//      seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 1,
//      seqKernelsPerThread -> 4,
//      kernelGroupSize -> 4,
//      windowTileWidth -> 1, windowTileHeight -> 1,
//      inputCacheSizeX -> 4, inputCacheSizeY -> 1,
//      inputCacheDepth -> 4,
//      kernelCacheSize -> 4,
//      localSizes(0) -> 8,
//      localSizes(1) -> 3,
//      localSizes(2) -> 7

      // debug_direct_autotune_4_cache_heur_0 (vectorizing illegally)
//       padOptRight -> 2, padOptBottom -> 5,
//       tileWidth -> 10, tileHeight -> 11, tileDepth -> 16,
//       seqWindowsPerThreadX -> 5, seqWindowsPerThreadY -> 1,
//       seqKernelsPerThread -> 4,
//       kernelGroupSize -> 8,
//       windowTileWidth -> 1, windowTileHeight -> 3,
//       inputCacheSizeX -> 5, inputCacheSizeY -> 1,
//       inputCacheDepth -> 1,
//       kernelCacheSize -> 4,
//       localSizes(0) -> 16,
//       localSizes(1) -> 1,
//       localSizes(2) -> 22

       // Our old best improved x2 for direct->GEMM + old layout (kernel_sharing_coalesce_fix) on GEMM L8:
//        padOptRight -> 4, padOptBottom -> 2,
//        tileWidth -> 32, tileHeight -> 3, tileDepth -> 64, //128,
//        seqWindowsPerThreadX -> 4, seqWindowsPerThreadY -> 3,
//        seqKernelsPerThread -> 2,
//        kernelGroupSize -> 2,
//        windowTileWidth -> 1, windowTileHeight -> 1,
//        inputCacheSizeX -> 4, inputCacheSizeY -> 1,
//        inputCacheDepth -> 8,
//        kernelCacheSize -> 2,
//        localSizes(0) -> 8, // (v_tileDepth_18*(1/^(v_inputCacheDepth_27))),          v_seqWindowsPerThreadX_20
//        localSizes(1) -> 6, // (v_kernelHeight_6*(1/^(v_windowTileHeight_24)))   ,    (v_seqWindowsPerThreadY_21*v_seqKernelsPerThread_22)
//        localSizes(2) -> 8 // (v_tileWidth_16*v_tileHeight_17*(1/^(v_seqWindowsPerThreadX_20))*(1/^(v_seqWindowsPerThreadY_21)))
      // (1+(((v_inputWidth_1+v_padOptRight_12+(-1*v_kernelWidth_5)+(2*v_padFuncX_3))) / (v_kernelStrideX_7)))
      // must be divisible by tileWidth
//    )
    val manualTuneParams = bestTuneParams(layerConfigIdx)._1 ++ (
      if (runConfig.chooseLocalSizeDeterministically)
        ListMap[Var, Int]()
      else bestTuneParams(layerConfigIdx)._2)

    if (debugRun)
      manualTuneParams.map { case (p, v) =>
        ParamConstraint(
          s"debugConstraint_${p.name}",
          f"${p.name} must equal ${v}",
          params = List(p, v),
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => args.head == args(1),
          predicate = "==")
      }.toVector
    else Vector()
  }
}
