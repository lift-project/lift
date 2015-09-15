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
            val str = TestHighLevelRewrite.dumpLambdaToMethod(expr).replace("idfloat", "id")
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

    var list = List[Lambda]()

    copiesAdded.foreach( x => {
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
    val idsAdded = addIds(lambda)

    addToAddressSpace(idsAdded, Rules.localMemoryId, 2)
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

    val depthMap = NumberExpression.byDepth(lambda).filterKeys({
      case FunCall(map: ir.ast.AbstractMap, _) if map.f.body.isConcrete => true
      case _ => false
    })

    val byDepth = depthMap.groupBy(_._2).mapValues(_.keys.toList).filter(pair => pair._2.exists({
      case FunCall(Map(_), _) => true
      case _ => false
    }))

    val depth = byDepth.map(pair =>{
      val level = pair._1
      val expressions = pair._2

      val (nonLowered, lowered) = expressions.partition({
        case FunCall(map: Map, _) => true
        case _ => false
      })

      (level, lowered.head, nonLowered)
    })

    val idMap = NumberExpression.breadthFirst(lambda)

    var lowered = lambda

    depth.foreach(tuple => {
      val toLower = tuple._3
      val lowerToType = tuple._2

      val rule = lowerToType match {
        case FunCall(_: MapSeq, _) => Rules.mapSeq
        case FunCall(MapLcl(dim, _), _) => Rules.mapLcl(dim)
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

