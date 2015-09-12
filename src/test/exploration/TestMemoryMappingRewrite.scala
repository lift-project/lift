package exploration

import apart.arithmetic.Var
import ir.{ArrayType, TupleType}
import ir.ast._
import opencl.ir.Float
import opencl.executor.{Executor, Eval}
import opencl.ir.{OpenCLMemory, OpenCLAddressSpace}
import opencl.ir.pattern.{toLocal, MapLcl, MapSeq}

import scala.io.Source

object TestMemoryMappingRewrite {

  def main(args: Array[String]) {
    Executor.loadLibrary()
    Executor.init()

    val all_files = Source.fromFile("list").getLines()

    var counter = 0

    all_files.toList.par.foreach(filename => {
      counter = counter + 1
      val hash = filename.split("/").last

      println(s"Lowering : ${hash} $counter / ${all_files.size}")

      val fileContents = Source.fromFile(filename).getLines.mkString("\n").replace("idfloat", "id")

      val lambda = Eval(fileContents)

      val lowered = Lower.lowerNoAddressSpaces(lambda)
      println(lowered)

      val mapped = mapAddressSpace(lowered)

      var id = 0
      mapped.foreach(expr => {
        id += 1
        try {

          val str = TestHighLevelRewrite.dumpLambdaToMethod(expr)
          val sha256 = TestHighLevelRewrite.Sha256Hash(str)
          val folder = s"lower/${hash}/" + sha256.charAt(0) + "/" + sha256.charAt(1)

          TestHighLevelRewrite.dumpToFile(str, sha256, folder)
        } catch {
          case t: Throwable =>
            println(s"No $id failed with ${t.toString.replaceAll("\n", " ")}.")
        }
      })
    })
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

    // generate all combinations for up to 3 toLocal
    val all_local_combinations = (1 to 3).map(idlist.combinations(_).toList).reduce(_++_)

    var tuned_expr = addedIds

    var all_mappings = List(addedIds)

    // Step 2: enumerate all possible mappings, including invalids
    /*idlist.foreach(node => {
      var all_new_mappings: List[Lambda] = List.empty
      all_mappings.foreach(x => {
        // Use local memory if there are enough resources
        if(OpenCLMemory.getMaxSizeInBytes(node.t).eval < Executor.getDeviceLocalMemSize())
          all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.localMemoryId) :: all_new_mappings

        // remove ID
        all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.dropId) :: all_new_mappings
      })

      all_mappings = all_new_mappings
    })*/

    // Function to add the lowered expression if there are enough resources
    def addToLocal(subset: List[Expr], curset: List[Lambda]): List[Lambda] = {
      // we start with the expression
      var tuned_expr = addedIds

      // traverse all the Id nodes
      idlist.foreach(node => {
        // if it is in the change set, we need to switch it to a local
        if (subset.contains(node)) {
          //val local_mem_size = OpenCLMemory.getMaxSizeInBytes(node.t).eval
          tuned_expr = Rewrite.applyRuleAt(tuned_expr, node, Rules.localMemoryId)
        }
        // otherwise we eliminate it
        else
          tuned_expr = Rewrite.applyRuleAt(tuned_expr, node, Rules.dropId)
      })

      tuned_expr :: curset
    }

    // for each substitution set
    all_local_combinations.foreach(subset => {
      all_mappings = addToLocal(subset, all_mappings)
    })

    // Step 3: remove invalid combinations

    // Step 4: make sure the end result is in global
    val lowered = all_mappings.flatMap(mapPrivateMemory)

    println(lowered.length)

    val total: List[Lambda] = lowered.flatMap(turnIdsIntoCopies).map(LowerMapInIds)

    println(total.length)

    total
  }

  private def mapPrivateMemory(lambda: Lambda): Seq[Lambda] = {

    ir.Context.updateContext(lambda.body)

    val (mapseq_list, _) = Expr.visitLeftToRight((List[Expr](), false))(lambda.body, (expr, pair) => {
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
    val all_private_combinations = (1 to 2).map(privateIdList.combinations(_).toList).reduce(_++_)

    var all_mappings_private = List(idsAdded)

    /*privateIdList.foreach(node => {

      var all_new_mappings: List[Lambda] = List.empty
      all_mappings_private.foreach(x => {
        ir.Context.updateContext(x.body)

        // Use private memory
        all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.privateMemoryId) :: all_new_mappings

        // remove ID
        all_new_mappings = Rewrite.applyRuleAt(x, node, Rules.dropId) :: all_new_mappings
      })

      all_mappings_private = all_new_mappings
    })*/

    def addToPrivate(subset: List[Expr], curset: List[Lambda]): List[Lambda] = {
      // we start with the expression
      var tuned_expr = idsAdded

      // traverse all the Id nodes
      privateIdList.foreach(node => {
        // if it is in the change set, we need to switch it to a local
        if (subset.contains(node)) {
          tuned_expr = Rewrite.applyRuleAt(tuned_expr, node, Rules.privateMemoryId)
        }
        // otherwise we eliminate it
        else
          tuned_expr = Rewrite.applyRuleAt(tuned_expr, node, Rules.dropId)
      })

      tuned_expr :: curset
    }

    all_private_combinations.foreach(subset => {
      all_mappings_private = addToPrivate(subset, all_mappings_private)
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
      println(ruleAt)
      addIds(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
    } else {
      lambda
    }
  }
}

