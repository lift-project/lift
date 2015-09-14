package exploration

import java.util.concurrent.atomic.AtomicInteger

import ir._
import ir.ast._
import opencl.executor.{Eval, Executor}
import opencl.ir.pattern._

import scala.io.Source

object TestMemoryMappingRewrite {

  def main(args: Array[String]) {
    Executor.loadLibrary()
    Executor.init()

    val all_files = Source.fromFile("list").getLines().toList

    val counter = new AtomicInteger()

    all_files.par.foreach(filename => {

      val count = counter.incrementAndGet()
      val hash = filename.split("/").last

      println(s"Lowering : $hash $count / ${all_files.size}")

      val fileContents = Source.fromFile(filename).getLines().mkString("\n").replace("idfloat", "id")

      try {

        val lambda = Eval(fileContents)

        val lowered = Lower.lowerNoAddressSpaces(lambda)

        val mapped = mapAddressSpaces(lowered)


        var id = 0
        mapped.foreach(expr => {
          id += 1

          try {

            // Sanity checks.
            if (expr.body.contains({ case FunCall(Id(), _) => }))
              throw new RuntimeException("Illegal Id")

            if (expr.body.contains({ case FunCall(Map(l), _) if l.body.isConcrete => }))
              throw new RuntimeException(s"Illegal un-lowered Map")

            // Dump to file
            val str = TestHighLevelRewrite.dumpLambdaToMethod(expr)
            val sha256 = TestHighLevelRewrite.Sha256Hash(str)
            val folder = s"lower/$hash/" + sha256.charAt(0) + "/" + sha256.charAt(1)

            TestHighLevelRewrite.dumpToFile(str, sha256, folder)
          } catch {
            case t: Throwable =>
              println(s"No $id of $count failed with ${t.toString}.")
          }
        })
      } catch {
        case t: Throwable =>
          println(s"Lowering $count failed with $t")
      }
    })
  }

  def mapAddressSpaces(lambda: Lambda): Seq[Lambda] = {

    val allLocalMappings = mapLocalMemory(lambda)

    val allPrivateMappings = allLocalMappings.flatMap(mapPrivateMemory)

    allPrivateMappings
  }

  def implementIds(lambdas: List[Lambda]): List[Lambda] = {
    val copiesAdded = lambdas.flatMap(turnIdsIntoCopies)
    copiesAdded.map(lowerMapInIds)
  }

  private def collectIds(addedIds: Lambda): List[Expr] =
    Expr.visitRightToLeft(List[Expr]())(addedIds.body, (expr, set) => {
      expr match {
        case FunCall(Id(), _) => expr :: set
        case _ => set
      }
    })

  def mapLocalMemory(lambda: Lambda): List[Lambda] = {
    // Step 1: Add id nodes in strategic locations
    val idsAdded = addIds(lambda)

    addToAddressSpace(idsAdded, Rules.localMemoryId, 3)
  }

  private def mapPrivateMemory(lambda: Lambda): List[Lambda] = {

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
          (call::pair._1, false)
        case _ =>
          pair
      }
    })

    val idsAddedToMapSeq =
      mapSeq.foldLeft(lambda)((l, x) => Rewrite.applyRuleAt(l, x, Rules.addId))

    val idsAdded = Rewrite.applyRulesUntilCannot(idsAddedToMapSeq, Seq(Rules.addIdForCurrentValueInReduce))

    addToAddressSpace(idsAdded, Rules.privateMemoryId, 2)
  }

  def addToAddressSpace(lambda: Lambda,
                        addressSpaceRule: Rule,
                        maxCombinations: Int): List[Lambda] = {
    val idLocations = collectIds(lambda)
    val combinations = getCombinations(idLocations, maxCombinations)

    val addedToAddressSpace = combinations.map(subset => {
      // traverse all the Id nodes
      idLocations.foldLeft(lambda)((tuned_expr, node) => {
        // if it is in the change set, we need to add toAddressSpace
        if (subset.contains(node) && addressSpaceRule.isDefinedAt(node))
          Rewrite.applyRuleAt(tuned_expr, node, addressSpaceRule)
        else // otherwise we eliminate it
          Rewrite.applyRuleAt(tuned_expr, node, Rules.dropId)
      })
    })

    implementIds(addedToAddressSpace)
  }

  /**
   *
   * @param lambda Lambda where to apply the transformation
   * @return
   */
  private def lowerMapInIds(lambda:Lambda): Lambda = {

    val neighbours = Expr.visitRightToLeft(List[(FunCall, FunCall)]())(lambda.body, (expr, list) => {
      expr match {
        case call1@FunCall(_, FunCall(toLocal(Lambda(_, call@FunCall(Map(_), _))), _)) =>
          (call1, call) :: list
        case call1@FunCall(_, FunCall(toPrivate(Lambda(_, call@FunCall(Map(_), _))), _)) =>
          (call1, call) :: list
        case call1@FunCall(_, FunCall(Zip(_), _, FunCall(toLocal(Lambda(_, call@FunCall(Map(_), _))), _))) =>
          (call1, call) :: list
        case call1@FunCall(_, FunCall(Zip(_), _, FunCall(toPrivate(Lambda(_, call@FunCall(Map(_), _))), _))) =>
          (call1, call) :: list
        case _ => list
      }
    })

    val origIdMap = NumberExpression.breadthFirst(lambda)

    val lowered = neighbours.foldLeft(lambda)((l, pair) => {
      val idMap = NumberExpression.breadthFirst(l)
      val asHere = Rewrite.getExprForId(l.body, origIdMap(pair._1), idMap)

      applySameMapRules(l, asHere, pair._2, idMap)
    })

    lowered
  }

  def applySameMapRules(lambda: Lambda,
                        asThese: Expr,
                        toHere: Expr,
                        idMap: collection.Map[Expr, Int]): Lambda = {

    val getBody: PartialFunction[Expr, Expr] = { case FunCall(m: FPattern, _*) => m.f.body}
    val concreteFPattern: PartialFunction[Expr, Unit] =
      { case FunCall(m : FPattern, _*) if m.f.body.isConcrete => }

    toHere match {
      case FunCall(Tuple(_), args@_*) =>

        args
          .filter(concreteFPattern.isDefinedAt)
          .foldLeft(lambda)((l, pair) => applySameMapRules(l, asThese, pair, idMap))

      case _ =>

        val matchRule = asThese match {
          case FunCall(_: AbstractMap, _) => asThese
          case FunCall(_: AbstractPartRed, _, _) => asThese
          case FunCall(fp: FPattern, _) => fp.f.body
        }

        val rule = matchRule match {
          case FunCall(_: MapSeq, _) => Rules.mapSeq
          case FunCall(_: ReduceSeq, _, _) => Rules.mapSeq
          case FunCall(MapLcl(dim, _), _) => Rules.mapLcl(dim)
          case FunCall(f, _*) => throw new NotImplementedError(f.getClass.toString)
        }

        val newLambda = Rewrite.applyRuleAtId(lambda, idMap(toHere), rule)

        val body1 = getBody(matchRule)
        val body2 = getBody(toHere)

        var map1 = Utils.getExprForPatternInCallChain(body1, concreteFPattern)
        val map2 = Utils.getExprForPatternInCallChain(body2, concreteFPattern)
        val tuple = Utils.getExprForPatternInCallChain(body2, { case FunCall(Tuple(_), _*) => })

        if (map1.isEmpty) {
          body1 match {
            case FunCall(Tuple(_), args@_*) =>
              if (args.exists(concreteFPattern.isDefinedAt))
                map1 = args.find(concreteFPattern.isDefinedAt)
            case _ =>
          }
        }

        if (map1.isDefined && map2.isDefined)
          applySameMapRules(newLambda, map1.get, map2.get, idMap)
        else if (map1.isDefined && tuple.isDefined)
          applySameMapRules(newLambda, map1.get, tuple.get, idMap)

        else
          newLambda
    }
  }

  /**
   * Turn all Id() into Map(id) with the correct number of maps
   *
   * @param lambda Lambda where to apply the transformation
   * @return
   */
  def turnIdsIntoCopies(lambda: Lambda): Seq[Lambda] = {
    val rewrites = Rewrite.listAllPossibleRewrites(lambda, Rules.implementIdAsDeepCopy)

    if (rewrites.nonEmpty) {
      val ruleAt = rewrites.head


      ruleAt._2.t match {
        case TupleType(_*) =>
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

          allCombinations.map(combination => FunDecl.replace(lambda, ruleAt._2, combination)).flatMap(turnIdsIntoCopies)
        case _ => turnIdsIntoCopies(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
      }

    } else {
      Seq(applyLoopFusionToTuple(lambda))
    }
  }

  private def applyLoopFusionToTuple(lambda: Lambda): Lambda =
    Rewrite.applyRulesUntilCannot(lambda, Seq(Rules.tupleMap))

  private def addIds(lambda: Lambda): Lambda =
    Rewrite.applyRulesUntilCannot(lambda, Seq(Rules.addIdForCurrentValueInReduce, Rules.addIdMapLcl))

  private def getCombinations(localIdList: List[Expr], max: Int): List[List[Expr]] =
    (0 to max).map(localIdList.combinations(_).toList).reduce(_ ++ _)

}

