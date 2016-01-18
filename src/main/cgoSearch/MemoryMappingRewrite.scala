package cgoSearch

import java.io.FileWriter
import java.util.concurrent.atomic.AtomicInteger

import exploration.utils.{NumberExpression, Utils}
import exploration.{Lower, Rewrite, Rule, Rules}
import ir._
import ir.ast._
import opencl.ir.pattern._

import scala.io.Source

object MemoryMappingRewrite {

  def main(args: Array[String]) {

    val topFolder = if (args.isEmpty) "lambdas" else args.head

    val all_files = Source.fromFile(s"$topFolder/index").getLines().toList

    val counter = new AtomicInteger()

    all_files.par.foreach(filename => {

      val count = counter.incrementAndGet()
      val hash = filename.split("/").last

      println(s"Lowering : $hash $count / ${all_files.size}")

      try {

        val lambda = GenerateOpenCL.readLambdaFromFile(filename)

        val loweredExpressions = Lower.mapCombinations(lambda)

        loweredExpressions.foreach(lowered => {

          try {
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
                val str = Utils.dumpLambdaToMethod(expr).replace("idfloat", "id")
                val sha256 = Utils.Sha256Hash(str)
                val folder = s"${topFolder}Lower/$hash/" + sha256.charAt(0) + "/" + sha256.charAt(1)

                if (Utils.dumpToFile(str, sha256, folder)) {
                  // Add to index if it was unique
                  synchronized {
                    val idxFile = new FileWriter(s"${topFolder}Lower/index", true)
                    idxFile.write(folder + "/" + sha256 + "\n")
                    idxFile.close()
                  }
                }
              } catch {
                case t: Throwable =>
                  println(s"No $id of $count failed with ${t.toString}.")
              }
            })
          } catch {
            case t: Throwable =>
              println(s"Address space mapping for $count failed")
          }

        })
      } catch {
        case t: scala.MatchError =>
          t.printStackTrace()
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

    var list = List[Lambda]()

    lambdas.foreach(x => {
      try {
        list = lowerMapInIds(x) :: list
      } catch {
        case t: Throwable => println("Id map lowering failure. Continuing...")
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

  def mapLocalMemory(lambda: Lambda): List[Lambda] = {
    // Step 1: Add id nodes in strategic locations
    val idsAdded = addIdsForLocal(lambda)

    val toAddressAdded = addToAddressSpace(idsAdded, Rules.localMemory, 2)
    val copiesAdded = toAddressAdded.flatMap(
      turnIdsIntoCopies(_, doTupleCombinations = false, doVectorisation = true))

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
        case FunCall(map: Map, _) => true
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
            case FunCall(toLocal(Lambda(_, body)), _) => expr :: list
            case _ => list
          }
        })

        val vectorised = tryToVectorize.foldLeft(tuple)((currentLambda, a) => {
          val vectorised = Rewrite.applyRulesUntilCannot(a, Seq(Rules.vectorize(4)))
          FunDecl.replace(currentLambda, a, vectorised)
        })

        if (!(vectorised eq tuple))
          return Seq(vectorised, tuple)
      } catch {
        case _: Throwable =>
      }
    }

      Seq(tuple)
    }
  }

  private def applyLoopFusionToTuple(lambda: Lambda): Lambda =
    Rewrite.applyRulesUntilCannot(lambda, Seq(Rules.tupleMap))

  private def addIdsForLocal(lambda: Lambda): Lambda = {
    val temp = Rewrite.applyRulesUntilCannot(lambda,
      Seq(Rules.addIdForCurrentValueInReduce, Rules.addIdMapLcl))

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

