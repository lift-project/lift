package exploration

import apart.arithmetic.{Prod, Cst, ArithExpr, Var}
import ir.view.View
import ir.{UndefType, ScalarType, Type, ArrayType}
import ir.ast._
import opencl.executor.{Executor, Execute}
import opencl.generator.{OpenCLCodeGen}
import opencl.ir._
import opencl.ir.pattern.{ReduceSeq, MapSeq}
import scala.collection.immutable
import scala.collection.mutable.{Set, Map => ScalaMap}
import scala.collection.immutable.{Map => ScalaImmMap}

/**
 * Prototype implementation of the last stage of the rewrite system.
 * This should be abstracted into a tuner API once working.
 *
 * The current implementation takes a re-written high-level expression,
 * which is free of OpenCL specific rules and hard coded parameters.
 *
 * The low level re-write system is composed of two steps:
 * - map parallelism to the execution model: High-level maps are transformed
 *   to MapGlb/Wrg/Seq with the appropriate dimensions
 * - parameter tuning: variables used in arithmetic expressions are replaced
 *   with actual values.
 *
 * The system should automate the exploration of parallelism mapping and
 * parameter space.
 * - The preliminary goal is a naive, exhaustive search of all legal
 *   combinations.
 */

/** Simple algebraic constraints based on divisibility
  */
object Constraint {
  
  /** Get the constant factor of an expression.
    * This is used to extract multiple. For example, we can set the input to be 64*N,
    * the constant factor will then be 64.
    * 
    * This is more restrictive than the cstFactor of some [[ArithExpr]] classes, because
    * we want to make sure a product doesn't contain division or negative powers.
    */
  def getConstantFactor(ae: ArithExpr): Option[Int] = ae match {
    case Cst(x) => Some(x)
    case p: Prod =>
      // we only consider products made from vars and constants
      if (!p.factors.exists {
        case v: Var => true
        case c: Cst => true
        case _ => false
      })
        Some(p.cstFactor.eval)
      else None
    case _ => None
  }
  
  /** Get all the possible Int divisors of an expression */
  def divisors(ae: ArithExpr): List[Int] = getConstantFactor(ae) match {
    case Some(x) => // If the LHS has a constant factor, find if there are valid RHS values
      (1 to x).filter{ y => x % y == 0 }.toList
      
    case _ => // if the LHS cannot be decomposed, there are no divisors 
      List.empty
  }
}

// Jump straight to the main function to see the steps

object TestLowLevelRewrite {

  /** Model the arithmetic constraints across the paramters */
  // TODO(tlutz): implement divisibility operator
  // The only constraints to far is divisibility (introduced by Split).
  // For now we use a Map where the Key has to be divisible by all of its values.
  // Ideally this should just be a system of equations. This requires a new
  // divisible operator ( A | B ) in arithExpr.
  type ConstraintMap = ScalaMap[ArithExpr, Set[ArithExpr]]

  // Alias for the substitution table
  type ValueMap = ScalaImmMap[Var, Int]

  case class ConstraintSystem(cm: ConstraintMap, vm: ValueMap) {
    // Propagate any value
    def replaceAny(): List[ConstraintSystem] = {
      List.empty
    }
  }

  // Debug flag
  val VERBOSE = true

  // == Rewrite functions ==

  /** Extract an arithmetic expression from an expression. */
  def extractArithExpr(expr: Expr): Option[ArithExpr] = expr match {
    case f@FunCall(s: Split, _) => Some(s.chunkSize)
    case f@FunCall(Scatter(ReorderWithStride(expr)), _) => Some(expr)
    case f@FunCall(Gather(ReorderWithStride(expr)), _) => Some(expr)

    // Sanity checks: [[ReorderWithStride]] is currently the only index function defined.
    // The two tests below are just sanity checks to introduce a failure in case we add other functions.
    case FunCall(_:Scatter,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case FunCall(_:Gather,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case _ => None
  }

  /** Filter function to find the nodes affected by parameters */
  def isParameteric(expr: Expr): Boolean = expr match {
    case _ if extractArithExpr(expr).isDefined => true
    case v: Value => true // this is necessary to propagate the parameters in the types
    case _ => false
  }

  /** List all the tunable parameters in the expression */
  def findTunableNodes(expr: Lambda): List[Expr] = {
    val replace_targets = Expr.visitWithState(List[Expr]())(expr.body, (x, set) => {
      if (isParameteric(x)) x :: set
      else set
    })

    if(VERBOSE) println(s"Found ${replace_targets.length} parameterizable nodes")

    replace_targets
  }

  /** Enumerate the constraints based on the list of tunable nodes */
  def FindParameterConstraints(nodes: List[Expr]): ConstraintMap = {
    var constraints: ConstraintMap = ScalaMap.empty

    nodes.foreach {
      case f@FunCall(Split(stride), x) =>
        // input length. If the expression is typechecked it is guaranteed to be an array
        val len = x.t.asInstanceOf[ArrayType].len
        constraints += len -> (constraints.getOrElse(len, Set[ArithExpr]()) + stride)
      case _ =>
    }

    if(VERBOSE) {
      println("Constraint system:")
      constraints.foreach(x => println(s" - ${x._1} must be divisible by ${x._2.mkString(" and ")}"))
    }

    constraints
  }

  /** Get a list of variables and their precedence */
  def findTuningKnobs(cm: ConstraintMap): Unit = {
    var var_dependencies = ScalaMap[Var,Set[Var]]()

    // traverse the constraint system and get all the variables
    var params = Set[Var]()
    cm.foreach(x => {
      params = params ++ x._1.varList
      x._2.foreach(y => params = params ++ y.varList)
    })

    if (VERBOSE) println(s"Found ${params.size} variables")

    // Rebuild the constraint map after propagating one value
    def Substitute(cm: ConstraintMap, from: ArithExpr, to:ArithExpr): ConstraintMap = {
      // create an immutable map with the substitution
      val sub = ScalaMap(from -> to).toMap
      // output map
      var out: ConstraintMap = ScalaMap.empty
      cm.foreach(x => {
        // compute the new key
        val key = ArithExpr.substitute(x._1, sub)
        // propagate in all the constraints and remove the constants
        val values = x._2.map(ArithExpr.substitute(_, sub)).filter{
          case c: Cst => false
          case _ => true
        }
        // if there are some constraints left, push them in the map
        if (values.nonEmpty)
          out += key -> values
      })
      out
    }

    def findAllValues(cm: ConstraintMap, vm: ValueMap): Unit = {

    }
  }

  // === Main ===

  def main (args: Array[String]) {

    // Start up the executor
    Executor.loadLibrary()
    Executor.init()

    // Prepare input and gold
    val mSize = 1024
    val kSize = 1024
    val nSize = 1024
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)
    val executor = new ExecutionHarness(gold.flatten)
    val values = Seq(matrixA.transpose, matrixB)

    // Step 1: Get the expression for the re-writer:
    val expr = getHighLevelExpression()

    // Step 2: Find the tunable nodes in the expression
    val tunableNodes = findTunableNodes(expr).reverse
    //                                        ^--- this is necessary to traverse from right to left

    { // Herein starts the hack for CGO deadline:
      // instead of using an algebraic solver to model the constraints, we tune only the Split nodes
      // from the right to the left of the expression. We first collect all the valid substitution tables
      // and then we merely plow through it.

      // Step 3: Isolate only the splits.
      // Note that we need a list of pairs instead of a map to keep the order
      val splits = tunableNodes.collect { case f@FunCall(Split(cs), x) => (cs, x.t.asInstanceOf[ArrayType].len) }

      // utility function to traverse the list of pairs and replace a Var by a Cst
      def propagate(splits: List[(ArithExpr, ArithExpr)], m: ScalaImmMap[ArithExpr, ArithExpr]): List[(ArithExpr, ArithExpr)] = {
        splits.map((x) => (ArithExpr.substitute(x._1, m), ArithExpr.substitute(x._2, m)))
      }

      var all_substitution_tables: List[ScalaImmMap[ArithExpr, ArithExpr]] = List.empty

      // recursively build the substitution table.
      // It takes the first node to tune and recurse with all its possible values.
      def substitute(splits: List[(ArithExpr, ArithExpr)], substitutions: ScalaImmMap[ArithExpr, ArithExpr]): Unit = {

        if (splits.nonEmpty) {
          splits.head match {
            // If the stride is not set and the input length is constant, compute all divisors
            case (v: Var, Cst(len)) =>
              (1 to len).filter {
                len % _ == 0
              }.foreach(x => substitute(propagate(splits.tail, ScalaImmMap(v -> x)), substitutions + (v -> x)))

            // If the input AND the stride are already set, make sure they are multiple
            case (Cst(chunk), Cst(len)) if len % chunk == 0 =>
              substitute(splits.tail, substitutions)

            // Otherwise we cannot set the parameter or the current combination is invalid
            case (x, y) => println(s"failed: $x, $y")
          }
        }
        else // if we propagated all the nodes, we have a (hopefully valid) substitution table
          all_substitution_tables = substitutions :: all_substitution_tables
      }

      // compute all the valid substitution tables
      substitute(splits, ScalaImmMap.empty)

      println(s"Found ${all_substitution_tables.size} valid parameter sets")
      if (false) all_substitution_tables.foreach(st => println(st.map(x => s"${x._1} -> ${x._2}").mkString("; ")))

      // Step whatever: run everything
      var counter = 0
      var passed = 0
      var skipped = 0
      var failed = 0
      var crashed = 0
      var best_time = Double.PositiveInfinity
      var all_times: List[Double] = List.empty
      var best_substitutions = all_substitution_tables.head
      all_substitution_tables.foreach(st => {
        var tuned_expr = expr

        // Quick and dirty substitution,
        // This relies on the reference on the nodes gathered in the original expression.
        // As long as we substitute from right to left, we do only shallow copies of the expression tree,
        // so it seems to work.
        tunableNodes.foreach(node => {
          tuned_expr = Lambda(tuned_expr.params, Expr.replace(tuned_expr.body, node, node match {
            case f@FunCall(s: Split, x) =>
              FunCall(Split(ArithExpr.substitute(s.chunkSize, st)), x)
            case f@FunCall(s@Scatter(idx: ReorderWithStride), x) =>
              FunCall(Scatter(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
            case f@FunCall(s@Gather(idx: ReorderWithStride), x) =>
              FunCall(Gather(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
            case v: Value =>
              Value(v.value, Type.substitute(v.t, st))
            case _ =>
              // If you end up here, it is most likely because of one of the following:
              // - a Scatter/Gather with an index function other than a ReorderWithStride
              throw new RuntimeException("Cannot substitute node")
          }))
        })

        counter = counter + 1
        //println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
        val (test, time) = executor(tuned_expr, values:_*)

        import ExecutionHarness.Status._
        test match {
          case Success =>
            passed = passed + 1
            all_times = time :: all_times
            if (time < best_time) {
              best_time = time
              println(s"New best time: ${best_time}")
              best_substitutions = st
            }

          case Skipped =>
            skipped = skipped + 1

          case ValidationError =>
            println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
            println(tuned_expr)
            failed = failed + 1

          case _ =>
            println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
            println(tuned_expr)
            crashed = crashed + 1
        }

        println(s"$counter / ${all_substitution_tables.size} ($passed passed, $skipped skipped, $failed failed, $crashed crashed)")
      })

      println("All times:")
      println(all_times.mkString(", "))
      println(s"best time: ${best_time}")
      println("best parameters: " + best_substitutions.map(x => s"${x._1} -> ${x._2}").mkString("; "))
    }


    
    // Step 4: Extract all variables from the constraint system and their dependencies
    //val parameters = findTuningKnobs(constraints)

    // Step 4: explore all possible values satisfying the constraints
    //val (localRange, globalRange) = InferNDRange(expr, values:_*)

    //executor(expr, globalRange, localRange, values:_*)

    // Cleanup
    Executor.shutdown()
  }

  // === Utils ===
  // The code below is not part of the low-level rewrite system


  /** This simulates the other part of the rewrite system.
    * It returns a high level expression free of OpenCL specific rules
    * and parameters.
    * @return An expression "generated" by the high-level rewrite system
    */
  def getHighLevelExpression(): Lambda = {
    val tileSizeMNVar = Var("tileSizeMNVar")
    val tileSizeKVar = Var("tileSizeKVar")
    val workPerThreadNVar = Var("workPerThreadNVar")
    val workPerThreadMVar = Var("workPerThreadMVar")

    /*val N = Var("N")
    val M = Var("M")
    val K = Var("K")*/
    val N = 1024
    val M = 1024
    val K = 1024

    val f0 =
      fun(
        ArrayType(ArrayType(Float, N), K),
        ArrayType(ArrayType(Float, M), K),
        (p1795960102, p477289012) =>
          FunCall(Join(),
            FunCall(Map(fun((p1889248251) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(Map(fun((p2023938592) =>
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p225290371) =>
                        FunCall(Scatter(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), p225290371))),
                        FunCall(Join(),
                          FunCall(Map(fun((p297927961) =>
                            FunCall(TransposeW(),
                              FunCall(Join(),
                                FunCall(Map(fun((p733672688) =>
                                  FunCall(TransposeW(),
                                    FunCall(Map(fun((p756185697) =>
                                      FunCall(TransposeW(), p756185697))),
                                      FunCall(TransposeW(), p733672688))))),
                                  FunCall(TransposeW(), p297927961)))))),
                            FunCall(TransposeW(),
                              FunCall(MapSeq(fun((p1691875296) =>
                                FunCall(Id(), p1691875296))),
                                FunCall(ReduceSeq(fun((p500179317, p1225197672) =>
                                  FunCall(Map(fun((p1500608548) =>
                                    FunCall(Map(fun((p513700442) =>
                                      FunCall(Map(fun((p912011468) =>
                                        FunCall(Join(),
                                          FunCall(Transpose(), p912011468)))),
                                        FunCall(Transpose(),
                                          FunCall(MapSeq(fun((p1195067075) =>
                                            FunCall(Id(), p1195067075))),
                                            FunCall(ReduceSeq(fun((p1983025922, p1007309018) =>
                                              FunCall(Map(fun((p2038148563) =>
                                                FunCall(Map(fun((p2142080121) =>
                                                  FunCall(add,
                                                    FunCall(Get(0), p2142080121),
                                                    FunCall(Get(1), p2142080121)))),
                                                  FunCall(Zip(2),
                                                    FunCall(Get(0), p2038148563),
                                                    FunCall(Map(fun((p112619572) =>
                                                      FunCall(mult,
                                                        FunCall(Get(1), p2038148563), p112619572))),
                                                      FunCall(Get(1), p1007309018)))))),
                                                FunCall(Zip(2), p1983025922,
                                                  FunCall(Get(0), p1007309018))))),
                                              FunCall(Get(0), p513700442),
                                              FunCall(Zip(2),
                                                FunCall(Transpose(),
                                                  FunCall(Get(1), p1500608548)),
                                                FunCall(Transpose(),
                                                  FunCall(Get(1), p513700442))))))))),
                                      FunCall(Zip(2),
                                        FunCall(Get(0), p1500608548),
                                        FunCall(Split(workPerThreadMVar),
                                          FunCall(Gather(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p1225197672)))))))),
                                    FunCall(Zip(2), p500179317,
                                      FunCall(Split(workPerThreadNVar),
                                        FunCall(Transpose(),
                                          FunCall(Get(0), p1225197672))))))),
                                  FunCall(Map(fun((p1786364562) =>
                                    FunCall(Map(fun((p326298949) =>
                                      FunCall(Map(fun((p876926621) =>
                                        FunCall(Map(fun((p1268959798) =>
                                          FunCall(id, p1268959798))), p876926621))), p326298949))), p1786364562))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadMVar), workPerThreadNVar), tileSizeMNVar/^workPerThreadMVar), tileSizeMNVar/^workPerThreadNVar))),
                                  FunCall(Zip(2), p1889248251, p2023938592)))))))))),
                    FunCall(Transpose(),
                      FunCall(Map(fun((p1935972447) =>
                        FunCall(Transpose(), p1935972447))),
                        FunCall(Split(tileSizeKVar),
                          FunCall(Map(fun((p1890627974) =>
                            FunCall(Split(tileSizeMNVar), p1890627974))), p477289012))))))))),
              FunCall(Transpose(),
                FunCall(Map(fun((p1641313620) =>
                  FunCall(Transpose(), p1641313620))),
                  FunCall(Split(tileSizeKVar),
                    FunCall(Map(fun((p192881625) =>
                      FunCall(Split(tileSizeMNVar), p192881625))), p1795960102)))))))

    // This is required to patch-up the address space modifiers for reduce
    Lower.lowerNoAddressSpaces(f0)
  }

  object ExecutionHarness {
    object Status extends Enumeration {
      type Status = Value
      val Success, Skipped, ValidationError, ArithmeticsError, ExecutorError, UnknwownError = Value
    }
  }

  /** Quick execution harness to restart the executor on failure. */
  class ExecutionHarness(gold: Array[Float]) {

    import ExecutionHarness.Status._

    // best execution time so far
    var best_time = scala.Float.PositiveInfinity

    var time: List[Double] = List.empty

    // Function called on successful execution and cross validation
    def success(time: Double): (Status, Double) = {
      (Success, time)
    }

    // Error callback
    def failure(reason: Status): (Status, Double) = (reason, 0.0)

    // run the given lambda with the given dimensions and parameters
    def apply(expr: Lambda, values: Any*): (Status, Double) = {
      try {
        val (local, global) = InferNDRange(expr, values:_*)

        // === Filtering ===
        // Remove obviously bad decisions based on some arbitrary constraints

        expr.params.foreach((p) => {
          p.t match {
            case _: ScalarType =>
              p.mem = OpenCLMemory.allocPrivateMemory(
                OpenCLMemory.getMaxSizeInBytes(p.t))
            case _ =>
              p.mem = OpenCLMemory.allocGlobalMemory(
                OpenCLMemory.getMaxSizeInBytes(p.t))
          }
          p.view = View(p.t, OpenCLCodeGen.print(p.mem.variable))
        })
        OpenCLMemoryAllocator.alloc(expr.body)
        val buffers = TypedOpenCLMemory.get(expr.body, expr.params, true)
        val private_buffers_size = buffers.filter(_.mem.addressSpace == PrivateMemory)
        if(private_buffers_size.map(x => OpenCLMemory.getMaxSizeInBytes(x.t)).reduce(_+_).eval > 8192)
          return failure(Skipped)

        // Rule out obviously poor choices based on the grid size
        if (local.map(_.eval).product < 16) return failure(Skipped)
        if (global.map(_.eval).product < 16) return failure(Skipped)
        if ((global.map(_.eval) zip local.map(_.eval)).map(x => x._1 / x._2).product < 4)
          return failure(Skipped)

        // Avoid crashing for invalid values
        if(local.map(_.eval).product > Executor.getDeviceMaxWorkGroupSize)
          return failure(Skipped)

        // === Execution ===
        val (output: Array[Float], time) =
          Execute (local(0).eval, local(1).eval, global(0).eval, global(1).eval, (true, true) ) (10, 100.0f, expr, values: _*)

        // cross validation
        if (output.length != gold.length)
          failure(ValidationError)
        else {
          /*val mismatch = (output zip gold).collect{
            case x if Math.abs(x._1 - x._2) > 0.001f * Math.max(Math.abs(x._1), Math.abs(x._2)) => x }.toList
          if (mismatch.isEmpty) success(time)*/
          val passed = (output zip gold).forall(x => Math.abs(x._1 - x._2) < 0.001f * Math.max(Math.abs(x._1), Math.abs(x._2)))
          if (passed) success(time)
          else {
            //println("Error: " + mismatch.size + " / " + gold.size + " mismatch")
            println("Local size: " + local.mkString(", "))
            println("Global size: " + global.mkString(", "))
            failure(ValidationError)
          }
        }
      } catch {
        case ea: Executor.ExecutorFailureException =>
          ea.consume()
          failure(ExecutorError)
        case e: Exception =>
          e.printStackTrace()
          failure(UnknwownError)
      }
    }
  }
}
