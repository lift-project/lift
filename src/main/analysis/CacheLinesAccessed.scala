package analysis

import analysis.CacheLinesAccessed.DeviceConfig
import com.typesafe.scalalogging.Logger
import core.generator.GenericAST.VarRef
import ir.ast.{AbstractMap, AbstractReduce, ArrayFromExpr, Expr, FPattern, FunCall, Lambda, UserFun}
import ir.view._
import ir.{Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst, RangeAdd, Var}
import opencl.generator.{NDRange, OclFunction, RangesAndCounts}
import opencl.ir.pattern.{MapSeqVector, ReduceWhileSeq}
import opencl.ir.{CollectTypedOpenCLMemory, GlobalMemory, InferOpenCLAddressSpace, LocalMemory, OpenCLMemoryAllocator, RemoveRedundantMemory, TypedOpenCLMemory, UndefAddressSpace}

class CacheLinesAccessed private(lambda: Lambda,
                                 localSizes: Vector[Int],
                                 globalSizes: Vector[Int],
                                 deviceConfig: DeviceConfig,
                                 skipViewBuilding: Boolean = false,
                                 verbose: Boolean = false) {
  private val logger = Logger(this.getClass)

  assert(localSizes.head >= 0)
  assert(localSizes.length == 3)
  assert(globalSizes.length == 3)

  val wrgSizes: Vector[Int] = localSizes.zip(globalSizes).map { case (l, g) => Math.ceil(g.toFloat / l).toInt }

  if (!skipViewBuilding)
    inferInputViews()

  val mems: Seq[TypedOpenCLMemory] = CollectTypedOpenCLMemory.asFlatSequence(lambda, includePrivate = true)
  val iterators: Predef.Map[Expr, List[Var]] = collectIterators(lambda.body, List())

  val userFunGlobalArgs: Seq[(UserFun, Seq[Expr])] = collectUserFunGlobalArgs(lambda.body)

  def apply(): Long = {

    val nCacheLinesReadByAccesses = userFunGlobalArgs.flatMap {
      case (uf, args) => args.map(getNCacheLinesPerAccess(uf, _, verbose))
    }

    val totalCacheLinesRead = nCacheLinesReadByAccesses.sum

    if (verbose) {
      logger.info("totalCacheLinesRead = %,d".format(totalCacheLinesRead))
    }

    totalCacheLinesRead
  }

  def inferInputViews(): Unit = {
    TypeChecker(lambda)
    InferOpenCLAddressSpace(lambda)
    OpenCLMemoryAllocator(lambda)
    RemoveRedundantMemory(lambda)

    RangesAndCounts(lambda,
      NDRange(localSizes.head, localSizes(1), localSizes(2)),
      NDRange(globalSizes.head, globalSizes(1), globalSizes(2)), Predef.Map())

    BuildDepthInfo(lambda.body)

    lambda.params.foreach((p) => {
      p.view = View(p.t, p.mem.variable)
    })
    InputView(lambda.body)

    assert(lambda.body.addressSpace != UndefAddressSpace)
    assert(lambda.body.view != NoView)
  }

  def collectIterators(expr: Expr,
                       iteratorStack: List[Var]): Predef.Map[Expr, List[Var]] = {
    expr match {
      case call: FunCall =>
        call.args.map( collectIterators(_, iteratorStack) ).reduce(_ ++ _) ++ (
          call.f match {
            case _: UserFun           => Predef.Map(call.args.map(_ -> iteratorStack): _*)
            case m: AbstractMap       => collectIterators(m.f.body, iteratorStack :+ m.loopVar)
            case mv: MapSeqVector     =>

              (if (mv.scalarPartNonEmpty)
                collectIterators(mv.fScalar.body, iteratorStack :+ mv.scalarLoopVar)
              else Seq()) ++ (
                if (mv.vectorPartNonEmpty)
                  collectIterators(mv.fVectorized.body, iteratorStack :+ mv.vectorLoopVar)
                else Seq())

            case rs: ReduceWhileSeq   => throw new NotImplementedError()
            case r: AbstractReduce    => collectIterators(r.f.body, iteratorStack :+ r.loopVar)
            case fp: FPattern         => collectIterators(fp.f.body, iteratorStack)
            case l: Lambda            => collectIterators(l.body, iteratorStack)
            case _                    => Predef.Map[Expr, List[Var]]()
          })

      case ArrayFromExpr(e) => throw new NotImplementedError()

      case _ => Predef.Map()
    }
  }

  def collectUserFunGlobalArgs(expr: Expr): Seq[(UserFun, Seq[Expr])] = {
    expr match {
      case call: FunCall =>

        val resultFromArgs =
          call.args.foldLeft[Seq[(UserFun, Seq[Expr])]](Seq())((x, arg) => {
            x ++ collectUserFunGlobalArgs(arg)
          })

        (call.f match {
          case uf: UserFun =>
            val result = Seq((uf, call.args.filter {
              case arg if arg.addressSpace.containsAddressSpace(GlobalMemory) => true
              case arg if deviceConfig.localMemIsGlobal && arg.addressSpace.containsAddressSpace(LocalMemory) => true
              case _ => false
            }))
            result

          case rs: ReduceWhileSeq => throw new NotImplementedError()

          case m: MapSeqVector =>
            val argsWithinFVectorized = if (m.vectorPartNonEmpty)
              collectUserFunGlobalArgs(m.fVectorized.body)
            else Seq()

            val argsWithinFScalar = if (m.scalarPartNonEmpty)
              collectUserFunGlobalArgs(m.fScalar.body)
            else Seq()

            argsWithinFVectorized ++ argsWithinFScalar

          case fp: FPattern => collectUserFunGlobalArgs(fp.f.body)
          case l: Lambda => collectUserFunGlobalArgs(l.body)
          case _ => Seq()
        }) ++ resultFromArgs

      case ArrayFromExpr(e) => throw new NotImplementedError()

      case e => Seq()
    }
  }

  /**
   * @return [0..1]
   */
  def getNCacheLinesPerAccess(userFun: UserFun,
                              arg: Expr,
                              verbose: Boolean = false): Long = {
    val cacheLinesReadPerQuadPerAccess: Int = getCacheLinesReadPerQuad(arg)

    val iterationCount = getQuadIterationCount(arg)
    val cacheLinesReadPerAccess = cacheLinesReadPerQuadPerAccess * iterationCount
    if (verbose) {
      logger.info(s"UserFun $userFun arg $arg:")
      logger.info("cacheLinesReadPerQuadPerAccess = %,d".format(cacheLinesReadPerQuadPerAccess))
      logger.info("iterationCount = %,d".format(iterationCount))
      logger.info("cacheLinesReadPerAccess = %,d".format(cacheLinesReadPerAccess))
    }

    cacheLinesReadPerAccess
  }

  def getCacheLinesReadPerQuad(arg: Expr): Int = {

    val accessIdxExpr: ArithExpr = ViewPrinter.emit(arg.view) match {
      case VarRef(_, _, Some(arithExpression), None) => arithExpression.content
      case VarRef(_, _, Some(arithExpression), Some(_)) => throw new NotImplementedError()
      case x => throw new IllegalStateException(s"$x")
    }

    val firstSeqIterIdxExpr = accessIdxExpr.visitAndRebuild {
      // Ignore Map(Lcl|Glb)(x) for now
      case v @ Var(_, RangeAdd(of: OclFunction, _, _))
        if of.name.equals("get_local_id") || of.name.equals("get_global_id") =>
        v

      // Replace MapWrg(*) iterators with 0
      case Var(_, RangeAdd(of: OclFunction, _, _)) if of.name.equals("get_group_id") =>
        Cst(0)

      // Replace MapSeq iterator with its first value
      case Var(_, RangeAdd(start: Cst, _, _)) =>
        start

      case v: Var =>
        throw new IllegalArgumentException(
          s"Encountered ArithExpr variable $v in the index expression $accessIdxExpr. CacheUsageEfficiency requires " +
            s"that index expressions only use iterator variables.")
      case ae => ae
    }

    // TODO: since this only considers the first thread, throw error on conditional edge indices resulting from Pad and alike
    val firstQuadIndices: Seq[Long] = (0 until 4).flatMap(i =>
      if (i >= localSizes.head) None
      else Some(
        firstSeqIterIdxExpr.visitAndRebuild {

          // Replace MapLcl(0) iterator with i
          case Var(_, RangeAdd(of: OclFunction, _, _)) if of.name.equals("get_local_id") =>
            Cst( getThreadIdxInDim(i, of.param, local = true) )

          // Replace Glb(0) iterator with i
          case Var(_, RangeAdd(of: OclFunction, _, _)) if of.name.equals("get_global_id") =>
            Cst( getThreadIdxInDim(i, of.param, local = false) )

          case ae => ae
        }.evalLong
      )).sorted

    assert(firstQuadIndices.nonEmpty)

    val valueSizeInMemory = mems.collectFirst {
      case TypedOpenCLMemory(mem, t) if mem == arg.mem =>
        Type.getAllocatedSize(Type.getValueType(t)).evalLong
    } match {
      case Some(s) => s
      case None => throw new IllegalStateException()
    }
    val vectorIndexMultiplier = getValueSize(arg) / valueSizeInMemory

    var currentCacheLineStartIdx = firstQuadIndices.head
    var nCacheLines = 1
    firstQuadIndices.tail.foreach( rightIdx => {

      val cacheLinesCoveredByPairOfLoads =
        Math.min(2, Math.ceil(
          ((rightIdx - currentCacheLineStartIdx + 1 * vectorIndexMultiplier) * valueSizeInMemory).toFloat /
            deviceConfig.cacheLineSizeInBytes)).toInt

      cacheLinesCoveredByPairOfLoads match {
        case 1 =>
        case 2 =>
          nCacheLines += 1
          currentCacheLineStartIdx = rightIdx
        case _ => throw new IllegalStateException()
      }
    })

    nCacheLines
  }

  final case class NoQuadsFitInLocalSizes(val message: String = "",
                                          private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  def getThreadIdxInDim(idx: Int, dim: Int, local: Boolean): Int = {
    val threadDimensions = if (local) localSizes else globalSizes

    if (idx >= localSizes.product)
      throw new NoQuadsFitInLocalSizes

    dim match {
      case 0 =>
        idx % threadDimensions(0)
      case 1 =>
        (idx / threadDimensions(0)) % threadDimensions(1)
      case 2 =>
        (idx / (threadDimensions(0) * threadDimensions(1))) % threadDimensions(2)
    }
  }

  def getValueSize(arg: Expr): Long = Type.getAllocatedSize(Type.getValueType(arg.t)).evalLong

  private def safeMult(left: Long, right: Long): Long =
    if (BigInt(left * right) > Long.MaxValue)
      throw new IllegalArgumentException
    else left * right

//  private def safeMult(left: Double, right: Double): Double =
//    if (BigInt(Math.ceil(left).toLong * Math.ceil(right).toLong) > Long.MaxValue)
//      throw new IllegalArgumentException
//    else left * right


  private def getQuadIterationCount(arg: Expr): Long = {
    val nIterations: List[Either[Long, Long]] = iterators(arg).flatMap {
      // Sequential iterator
      case Var(_, r @ RangeAdd(_: Cst, _, _)) =>
        List(Right(r.numVals.evalLong))

      // Local iterator
      // NB: this is approximate: with local size of 180 and loop bound of 200, this will
      // calc number of seq iterations as 2
      case Var(_, r @ RangeAdd(of: OclFunction, stop, _)) if of.name.equals("get_local_id") =>
        //        List(Left(stop.evalLong))
        val parIterations = Math.min(of.range.asInstanceOf[RangeAdd].stop.evalLong, stop.evalLong)
//        val seqIterations = stop.evalDouble / parIterations
        val seqIterations = Math.ceil(stop.evalDouble / parIterations).toLong
        List(Left(parIterations), Right(seqIterations))

      // Wrg iterator
      case Var(_, r @ RangeAdd(of: OclFunction, stop, _)) if of.name.equals("get_group_id") =>
        List(Right(stop.evalLong)) // Math.min(wrgSizes(of.param), stop.evalInt).toLong

      // Global iterator
      case Var(_, r @ RangeAdd(of: OclFunction, stop, _)) if of.name.equals("get_global_id") =>
        List(Right(stop.evalLong)) // Math.min(wrgSizes(of.param) * localSizes(of.param), stop.evalInt).toLong
    }

    val (localIterations: List[Long], seqAndNonLocalParIterations: List[Long]) = nIterations.map {
      case Left(l) => (Some(l), None)
      case Right(d) => (None, Some(d))
    }.unzip match {
      case (x, y) => (x.flatten, y.flatten)
    }

//    Math.ceil(
      safeMult(
        Math.ceil((localIterations match {
          case Nil => 1
          case iters => iters.foldLeft(1L)(safeMult)
        }).toDouble / 4).toLong,

        seqAndNonLocalParIterations match {
          case Nil => 1
          case iters => iters.foldLeft(1L)(safeMult)
        })//).toLong
  }
}

object CacheLinesAccessed {

  case class DeviceConfig(nCores: Int,
                          nParQuadsPerCore: Int,
                          cacheLineSizeInBytes: Int,
                          localMemIsGlobal: Boolean) {
    val parallelQuadCapacity: Int = nCores * nParQuadsPerCore
  }

  def apply(lambda: Lambda,
            localSizes: (Int, Int, Int),
            globalSizes: (Int, Int, Int),
            deviceConfig: DeviceConfig,
            skipViewBuilding: Boolean = false,
            verbose: Boolean = false): Long =
    new CacheLinesAccessed(lambda,
      Vector(localSizes._1, localSizes._2, localSizes._3),
      Vector(globalSizes._1, globalSizes._2, globalSizes._3),
      deviceConfig, skipViewBuilding, verbose)()
}
