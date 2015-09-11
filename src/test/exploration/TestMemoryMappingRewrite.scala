package exploration

import ir.ast._
import opencl.executor.Eval
import opencl.ir.OpenCLAddressSpace
import opencl.ir.pattern.{MapLcl, MapSeq}

import scala.io.Source

object TestMemoryMappingRewrite {
  object Context {
    val inputs: scala.collection.Map[Expr, OpenCLAddressSpace] = scala.collection.Map.empty

    def push() = {

    }
  }

  def main(args: Array[String]) {
    val filename = "lambda_0_1441965545755"

    val fileContents = Source.fromFile(filename).getLines.mkString("\n").replace("idfloat", "id")

    val lambda = Eval(fileContents)

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
        val local_memory = Rewrite.applyRuleAt(x, node, Rules.localMemoryId)
        val no_local_memory = Rewrite.applyRuleAt(x, node, Rules.dropId)
        all_new_mappings = local_memory :: no_local_memory :: all_new_mappings
      })

      all_mappings = all_new_mappings
    })

    // Step 3: remove invalid combinations

    // Step 4: make sure the end result is in global

    all_mappings.map(turnIdsIntoCopies).map(LowerMapInIds)
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

    println(byDepth)

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

  def turnIdsIntoCopies(lambda: Lambda): Lambda = {
    val rewrites = Rewrite.listAllPossibleRewrites(lambda, Rules.implementIdAsDeepCopy)

    if (rewrites.nonEmpty) {
      val ruleAt = rewrites.head
      turnIdsIntoCopies(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
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
