package exploration

import ir.TupleType
import ir.ast._
import opencl.executor.{Executor, Eval}
import opencl.ir.{OpenCLMemory, OpenCLAddressSpace}
import opencl.ir.pattern.{toLocal, MapLcl, MapSeq}

import scala.io.Source

object TestMemoryMappingRewrite {

  def main(args: Array[String]) {
    Executor.loadLibrary()
    Executor.init()

    val filename = "lambda_0_1441965545755"

    val fileContents = Source.fromFile(filename).getLines.mkString("\n").replace("idfloat", "id")

    val lambda = Eval(fileContents)

    mapAddressSpace(lambda)

  }

  def mapAddressSpace(lambda: Lambda): Seq[Lambda] = {

    // Step 1: Add id nodes in strategic locations
    val addedIds = addIds(lambda)

    val idlist = Expr.visitRightToLeft(List[Expr]())(addedIds.body, (expr, set) => {
      expr match {
        case call@FunCall(Id(), _) =>
          call :: set
        case _ =>
          set
      }
    })

    var tuned_expr = addedIds

    var all_mappings = List(addedIds)

    // Step 2: enumerate all possible mappings, including invalids
    idlist.foreach(node => {
      var all_new_mappings: List[Lambda] = List.empty
      all_mappings.foreach(x => {
        // Use local memory if there are enough resources
        if(OpenCLMemory.getMaxSizeInBytes(node.t).eval < Executor.getDeviceLocalMemSize())
          all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.localMemoryId) :: all_new_mappings

        // remove ID
        all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.dropId) :: all_new_mappings
      })

      all_mappings = all_new_mappings
    })

    // Step 3: remove invalid combinations

    // Step 4: make sure the end result is in global


    all_mappings.flatMap(mapPrivateMemory).flatMap(turnIdsIntoCopies).map(LowerMapInIds)
  }

  private def mapPrivateMemory(lambda: Lambda): Seq[Lambda] = {

    ir.Context.updateContext(lambda.body)

    val (mapseq_list, _) = Expr.visitLeftToRight((List[Expr](), false))(lambda.body, (expr, pair) => {
      expr match {
        case FunCall(toLocal(_), _) =>
          (pair._1, true)
        case l@FunCall(MapSeq(_), _) if !pair._2 && l.context.inMapLcl.reduce(_ || _) =>
          (l::pair._1, false)
        case _ =>
          pair
      }
    })

    var idsAdded = lambda

    mapseq_list.foreach(x => {
      idsAdded = Rewrite.applyRuleAt(idsAdded, x, Rules.addId)
    })

    val privateIdList = Expr.visitLeftToRight(List[Expr]())(idsAdded.body, (expr, set) => {
      expr match {
        case FunCall(MapSeq(_), call@FunCall(Id(), _)) =>
          call :: set
        case _ =>
          set
      }
    })

    var all_mappings_private = List(idsAdded)

    privateIdList.foreach(node => {

      var all_new_mappings: List[Lambda] = List.empty
      all_mappings_private.foreach(x => {
        ir.Context.updateContext(x.body)

        // Use private memory
        all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.privateMemoryId) :: all_new_mappings

        // remove ID
        all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.dropId) :: all_new_mappings
      })

      all_mappings_private = all_new_mappings
    })

    all_mappings_private
  }

  private def LowerMapInIds(lambda:Lambda): Lambda = {
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

  def applyLoopFusionToTuple(lambda: Lambda): Lambda = {
    val rewrites = Rewrite.listAllPossibleRewrites(lambda, Rules.tupleMap)

    if (rewrites.nonEmpty) {
      val ruleAt = rewrites.head
      applyLoopFusionToTuple(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
    } else {
      lambda
    }
  }

  def addIds(lambda: Lambda): Lambda = {
    val rewrites = Rewrite.listAllPossibleRewritesForRules(lambda, Seq(Rules.addIdForCurrentValueInReduce, Rules.addIdMapLcl))

    if (rewrites.nonEmpty) {
      val ruleAt = rewrites.head
      addIds(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
    } else {
      lambda
    }
  }
}

