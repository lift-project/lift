package exploration

import java.io.{File, FileWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import ir._
import ir.ast._
import opencl.ir.pattern._
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.{ArgotParser, ArgotUsageException}
import rewriting.utils.{NumberExpression, Utils}
import rewriting._

import scala.io.Source

object MemoryMappingRewrite {

  private val logger = Logger(this.getClass)

  private val parser = new ArgotParser("MemoryMappingRewrite")

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, _) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  private val defaultVectorWidth = 4

  private val vectorWidth = parser.option[Int](List("vector-width", "vw"), "vector width",
    s"The vector width to use for vectorising rewrites. Default: $defaultVectorWidth")

  private val sequential = parser.flag[Boolean](List("s", "seq", "sequential"),
    "Don't execute in parallel.")

  private val loadBalancing = parser.flag[Boolean](List("l", "lb", "load-balancing"),
    "Enable load balancing using MapAtomLocal and MapAtomWrg")

  private val global0 = parser.flag[Boolean](List("gl0", "global0"),
    "Mapping: MapGlb(0)( MapSeq(...) )")

  private val global01 = parser.flag[Boolean](List("gl01", "global01"),
    "Mapping: MapGlb(0)(MapGlb(1)( MapSeq(...) ))")

  private val global10 = parser.flag[Boolean](List("gl10", "global10"),
    "Mapping: MapGlb(1)(MapGlb(0)( MapSeq(...) ))")

  private val global012 = parser.flag[Boolean](List("gl012", "global012"),
    "Mapping: MapGlb(0)(MapGlb(1)(MapGlb(2)( MapSeq(...) )))")

  private val global210 = parser.flag[Boolean](List("gl210", "global210"),
    "Mapping: MapGlb(2)(MapGlb(1)(MapGlb(0)( MapSeq(...) )))")

  private val group0 = parser.flag[Boolean](List("gr0", "group0"),
    "Mapping: MapWrg(0)(MapLcl(0)( MapSeq(...) ))")

  private val group01 = parser.flag[Boolean](List("gr01", "group01"),
    "Mapping: MapWrg(0)(MapWrg(1)(MapLcl(0)(MapLcl(1)( MapSeq(...) ))))")

  private val group10 = parser.flag[Boolean](List("gr10", "group10"),
    "Mapping: MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)( MapSeq(...) ))))")


  def main(args: Array[String]): Unit = {

    try {

      parser.parse(args)
      val enabledMappings =
        EnabledMappings(
          global0.value.isDefined,
          global01.value.isDefined,
          global10.value.isDefined,
          global012.value.isDefined,
          global210.value.isDefined,
          group0.value.isDefined,
          group01.value.isDefined,
          group10.value.isDefined
        )

      if(!enabledMappings.isOneEnabled) scala.sys.error("No mappings enabled")

      logger.info(s"Arguments: ${args.mkString(" ")}")
      logger.info("Defaults:")
      logger.info(s"\tVector width: $defaultVectorWidth")
      logger.info(s"\tMappings: $enabledMappings")

      val topFolder = input.value.get

      val all_files = Source.fromFile(s"$topFolder/index").getLines().toList

      val counter = new AtomicInteger()

      val allFilesPar = if (sequential.value.isDefined) all_files else all_files.par

      allFilesPar.foreach(filename => {

        val count = counter.incrementAndGet()
        val hash = filename.split("/").last
        val numLambdas = all_files.size

        print(s"\rLowering : $count / $numLambdas")

        try {

          val lambda = ParameterRewrite.readLambdaFromFile(filename)
          val lowered = lowerLambda(lambda, enabledMappings, topFolder)

          lowered.foreach(dumpToFile(topFolder, hash, _))

        } catch {
          case t: Throwable =>
            logger.warn(s"Reading $hash from file", t)
        }
      })

      println()

    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def lowerLambda(lambda: Lambda, enabledMappings: EnabledMappings, hash: String = "") = {

    try {

      val loweredExpressions = Lower.mapCombinations(lambda, enabledMappings)

      loweredExpressions.flatMap(
        mapAddressSpaces(_, hash).flatMap(addressMapped => {

          val loadBalanced = mapLoadBalancing(addressMapped, hash)
          filterIllegals(loadBalanced)

        })
      )

    } catch {
      case t: Throwable =>
        logger.warn(s"Lowering $hash failed.", t)
        Seq()
    }
  }

  def filterIllegals(lambdaSeq: Seq[Lambda], hash: String = "") = {

    lambdaSeq.filter(lambda =>
      try {
        // Sanity checks.
        if (lambda.body.contains({ case FunCall(Id(), _) => }))
          throw new RuntimeException("Illegal Id")

        if (lambda.body.contains({ case FunCall(Map(l), _) if l.body.isConcrete => }))
          throw new RuntimeException(s"Illegal un-lowered Map")

        true
      } catch {
        case t: Throwable =>
          //noinspection SideEffectsInMonadicTransformation
          logger.warn(s"Illegal lambda in $hash failed, ${t.toString}")
          false
      })
  }

  def dumpToFile(topFolder: String, hash: String, expr: Lambda): Unit = {
    // Dump to file
    val str = Utils.dumpLambdaToMethod(expr)
    val sha256 = Utils.Sha256Hash(str)
    val folder = s"${topFolder}Lower/$hash/" + sha256.charAt(0) + "/" + sha256.charAt(1)

    if (Utils.dumpToFile(str, sha256, folder)) {
      // Add to index if it was unique
      synchronized {
        val idxFile = new FileWriter(s"${topFolder}Lower/$hash/index", true)
        idxFile.write(folder + "/" + sha256 + "\n")
        idxFile.close()
      }
    }
  }

  /**
    * Returns all combinations with at most one load balancing rule applied
    *
    * @param lambda The lambda where to apply load balancing.
    */
  def  mapLoadBalancing(lambda: Lambda, hash: String): Seq[Lambda] = {
    if (loadBalancing.value.isEmpty)
      return Seq(lambda)

    try {
      val locations =
        Rewrite.listAllPossibleRewritesForRules(lambda, Seq(Rules.mapAtomWrg, Rules.mapAtomLcl))

      val withLoadBalancing = locations.map(pair => Rewrite.applyRuleAt(lambda, pair._2, pair._1))
      withLoadBalancing :+ lambda
    } catch {
      case _: Throwable =>
        logger.warn(s"Load balancing for $hash failed.")
        Seq()
    }
  }

  def mapAddressSpaces(lambda: Lambda, hash: String): Seq[Lambda] = {
    try {

      val allLocalMappings = mapLocalMemory(lambda)

      val allPrivateMappings = allLocalMappings.flatMap(mapPrivateMemory)

      allPrivateMappings
    } catch {
      case _: Throwable =>
        logger.warn(s"Address space mapping for $hash failed.")
       Seq()
    }
  }

  def implementIds(lambdas: List[Lambda]): List[Lambda] = {

    var list = List[Lambda]()

    lambdas.foreach(x => {
      try {
        list = lowerMapInIds(x) :: list
      } catch {
        case t: Throwable =>
         logger.warn("Id map lowering failure. Continuing...", t)
      }
    })

    list
  }

  private def collectIds(addedIds: Lambda): List[Expr] =
    Expr.visitRightToLeft(List[Expr]())(addedIds.body, (expr, set) => {
      expr match {
        case FunCall(Id(), _) => expr :: set
        case _ => set
      }
    })

  def mapLocalMemory(lambda: Lambda, doVectorisation: Boolean = true): List[Lambda] = {
    // Step 1: Add id nodes in strategic locations
    val idsAdded = addIdsForLocal(lambda)

    val toAddressAdded = addToAddressSpace(idsAdded, Rules.localMemory, 2)
    val copiesAdded = toAddressAdded.flatMap(
      turnIdsIntoCopies(_, doTupleCombinations = false, doVectorisation))

    val addedUserFun = addToAddressSpaceToUserFun(copiesAdded) ++ copiesAdded

    implementIds(addedUserFun)
  }

  // Try adding toLocal to user functions that are arguments to other user functions
  // and that would otherwise be forced to global
  private def addToAddressSpaceToUserFun(copiesAdded: List[Lambda]): List[Lambda] = {
    copiesAdded.map(f => {
      Context.updateContext(f.body)
      val res = Expr.visitLeftToRight(List[Expr]())(f.body, (e, s) => {
        e match {
          case FunCall(toGlobal(Lambda(_, FunCall(m: AbstractMap, _))), _) =>
            m.f.body match {
              case FunCall(_: UserFun, args@_*) =>
                args.collect({ case call@FunCall(_: UserFun, _*) => call }).toList ++ s
              case _ => s
            }
          case _ => s
        }
      }).filter(Rules.localMemory.isDefinedAt)

      Some(res.foldLeft(f)((l, expr) => Rewrite.applyRuleAt(l, expr, Rules.localMemory)))
    }).collect({ case Some(s) => s })
  }

  def mapPrivateMemory(lambda: Lambda): List[Lambda] = {

    ir.Context.updateContext(lambda.body)

    val (mapSeq, _) = Expr.visitLeftToRight((List[Expr](), false))(lambda.body, (expr, pair) => {
      expr match {
        case FunCall(toLocal(_), _) =>
          (pair._1, true)
        case call@FunCall(MapSeq(l), _)
          if !pair._2
            && call.context.inMapLcl.reduce(_ || _)
            && !l.body.contains({ case FunCall(uf: UserFun, _) if uf.name.startsWith("id") => })
        =>
          (call :: pair._1, false)
        case _ =>
          pair
      }
    })

    val idsAddedToMapSeq =
      mapSeq.foldLeft(lambda)((l, x) => Rewrite.applyRuleAt(l, x, Rules.addId))

    val idsAdded = Rewrite.applyRulesUntilCannot(idsAddedToMapSeq, Seq(Rules.addIdForCurrentValueInReduce))

    val toAddressAdded = addToAddressSpace(idsAdded, Rules.privateMemory, 2)
    val copiesAdded = toAddressAdded.flatMap(
      turnIdsIntoCopies(_, doTupleCombinations = true, doVectorisation = false))

    implementIds(copiesAdded)
  }

  def addToAddressSpace(lambda: Lambda,
                        addressSpaceRule: Rule,
                        maxCombinations: Int): List[Lambda] = {
    val idLocations = collectIds(lambda)
    val combinations = getCombinations(idLocations, maxCombinations)

    combinations.map(subset => {
      // traverse all the Id nodes
      idLocations.foldLeft(lambda)((tuned_expr, node) => {
        // if it is in the change set, we need to add toAddressSpace
        if (subset.contains(node) && addressSpaceRule.isDefinedAt(node)) {
          Rewrite.applyRuleAt(tuned_expr, node, addressSpaceRule)
        } else // otherwise we eliminate it
          Rewrite.applyRuleAt(tuned_expr, node, Rules.dropId)
      })
    })
  }

  /**
   *
   * @param lambda Lambda where to apply the transformation
   * @return
   */
  private def lowerMapInIds(lambda: Lambda): Lambda = {

    val depthMap = NumberExpression.byDepth(lambda).filterKeys({
      case FunCall(map: ir.ast.AbstractMap, _) if map.f.body.isConcrete => true
      case _ => false
    })

    val byDepth = depthMap.groupBy(_._2).mapValues(_.keys.toList).filter(pair => pair._2.exists({
      case FunCall(Map(_), _) => true
      case _ => false
    }))

    val depth = byDepth.map(pair => {
      val level = pair._1
      val expressions = pair._2

      val (nonLowered, lowered) = expressions.partition({
        case FunCall(_: Map, _) => true
        case _ => false
      })

      (level, lowered, nonLowered)
    })

    val idMap = NumberExpression.breadthFirst(lambda)

    var lowered = lambda

    depth.foreach(tuple => {
      val toLower = tuple._3
      val lowerToType = tuple._2

      val rule = lowerToType match {
        case FunCall(_: MapSeq, _) :: _ => Rules.mapSeq
        case FunCall(MapLcl(dim, _), _) :: _ => Rules.mapLcl(dim)
        case _ => Rules.mapSeq // Fall back to seq
      }

      toLower.foreach(expr => {
        val id = idMap(expr)
        lowered = Rewrite.applyRuleAtId(lowered, id, rule)

      })
    })

    lowered
  }

  /**
   * Turn all Id() into Map(id) with the correct number of maps
   *
   * @param lambda Lambda where to apply the transformation
   * @return
   */
  def turnIdsIntoCopies(lambda: Lambda,
                        doTupleCombinations: Boolean,
                        doVectorisation: Boolean): Seq[Lambda] = {
    val rewrites = Rewrite.listAllPossibleRewrites(lambda, Rules.implementIdAsDeepCopy)

    if (rewrites.nonEmpty) {
      val ruleAt = rewrites.head

      ruleAt._2.t match {
        case TupleType(_*) if doTupleCombinations =>
          val oneLevelImplemented = Rules.implementOneLevelOfId.rewrite(ruleAt._2)

          val idRewrites = Rewrite.listAllPossibleRewrites(oneLevelImplemented, Rules.implementIdAsDeepCopy)

          var allCombinations = List[Expr]()

          (1 to idRewrites.length).foreach(n => {
            idRewrites.combinations(n).foreach(combination => {
              var copiesAdded = oneLevelImplemented

              combination.foreach(ruleAt2 => {
                copiesAdded = Rewrite.applyRuleAt(copiesAdded, ruleAt2._1, ruleAt2._2)
              })

              val dropLocations = Rewrite.listAllPossibleRewrites(copiesAdded, Rules.dropId)

              val droppedIds = dropLocations.foldLeft(copiesAdded)((a, b) => {
                Rewrite.applyRuleAt(a, b._1, b._2)
              })

              allCombinations = droppedIds :: allCombinations
            })
          })

          allCombinations
            .map(FunDecl.replace(lambda, ruleAt._2, _))
            .flatMap(turnIdsIntoCopies(_, doTupleCombinations, doVectorisation))
        case _ =>
          turnIdsIntoCopies(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1),
            doTupleCombinations,
            doVectorisation)
      }

    } else {
      val tuple = applyLoopFusionToTuple(lambda)

      if (doVectorisation) {

        try {

        val tryToVectorize = Expr.visitLeftToRight(List[Expr]())(tuple.body, (expr, list) => {
          expr match {
            case FunCall(toLocal(Lambda(_, _)), _) => expr :: list
            case _ => list
          }
        })

        val vectorised = tryToVectorize.foldLeft(tuple)((currentLambda, a) => {
          val width = vectorWidth.value.getOrElse(defaultVectorWidth)
          val vectorised =
            Rewrite.applyRulesUntilCannot(
              a,
              Seq(Rules.vectorize(width))
            )

          FunDecl.replace(currentLambda, a, vectorised)
        })

        if (!(vectorised eq tuple))
          return Seq(vectorised, tuple)
      } catch {
        case _: Throwable =>
            // TODO: log?
      }
    }

      Seq(tuple)
    }
  }

  private def applyLoopFusionToTuple(lambda: Lambda): Lambda =
    Rewrite.applyRulesUntilCannot(lambda, Seq(Rules.tupleMap))

  private def addIdsForLocal(lambda: Lambda): Lambda = {
    val temp = Rewrite.applyRulesUntilCannot(lambda,
      Seq(Rules.addIdForCurrentValueInReduce, Rules.addIdMapLcl, Rules.addIdMapWrg))

    val reduceSeqs = Expr.visitLeftToRight(List[Expr]())(temp.body, (e, s) =>
      e match {
        case call@FunCall(_: ReduceSeq, _*) => call :: s
        case _ => s
      }).filterNot(e => temp.body.contains({ case FunCall(toGlobal(Lambda(_, c)), _*) if c eq e => }))

    reduceSeqs.foldRight(temp)((e, l) => Rewrite.applyRuleAt(l, e, Rules.addIdAfterReduce))
  }

  private def getCombinations(localIdList: List[Expr], max: Int): List[List[Expr]] =
    (0 to max).map(localIdList.combinations(_).toList).reduce(_ ++ _)

}

