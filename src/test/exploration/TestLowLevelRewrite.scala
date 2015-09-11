package exploration

import apart.arithmetic.{ArithExpr, Cst, Prod, Var}
import ir.ast._
import ir.view.View
import ir.{Type, ArrayType, ScalarType, TypeChecker}
import opencl.executor.{Execute, Executor}
import opencl.generator.{OpenCLGenerator, OpenCLCodeGen}
import opencl.ir._
import opencl.ir.pattern._

import scala.collection.immutable.{Map => ScalaImmMap}
import scala.collection.mutable.{Map => ScalaMap, Set}

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

object AppParams {
  // matrix size
  val matrix_size = 4096

  // Minimum number of work item per workgroup
  val min_work_items = 16

  // Minimal global grid size
  val min_grid_size = 4

  // Max amount of private memory allocated (this is not necessarily the number of registers)
  val max_amount_private_memory = 8192*5

  // Minimum number of workgroups
  val min_num_workgroups = 4

  // Maximum number of workgroups
  val max_num_workgroups = 10000

  // Fraction of the max local memory allocated to a single work item
  val resource_per_thread = 1.0

  // Don't bother cross validating if the timing is not better than the current best solution
  val only_crossvalidate_better_solutions = true
}

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

  def lowlevelexecutor (lambdas: List[Lambda]) {

    // Start up the executor
    Executor.loadLibrary()
    Executor.init()

    // Prepare input and gold
    val mSize = AppParams.matrix_size
    val kSize = AppParams.matrix_size
    val nSize = AppParams.matrix_size
    println("Generating data")
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    println("Computing gold solution")
    val gold = Executor.nativeMatrixMultiply(
      matrixA.flatten.map(_.asInstanceOf[java.lang.Float]),
      matrixB.flatten.map(_.asInstanceOf[java.lang.Float]), mSize, nSize, kSize).map(_.toFloat)

    val executor = new ExecutionHarness(gold)
    val values = Seq(matrixA.transpose, matrixB)

    lambdas.foreach(expr => {
      replaceInputTypes(expr)
      TypeChecker(expr)
      println(expr)

    // Step 2: Find the tunable nodes in the expression
      val tunableNodes = Utils.findTunableNodes(expr)

      if(VERBOSE) println(s"Found ${tunableNodes.length} parameterizable nodes")

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

        println("Building substitution tables")

        // recursively build the substitution table.
        // It takes the first node to tune and recurse with all its possible values.
        def substitute(splits: List[(ArithExpr, ArithExpr)], substitutions: ScalaImmMap[ArithExpr, ArithExpr]): Unit = {

          if (splits.nonEmpty) {
            splits.head match {
              // If the stride is not set and the input length is constant, compute all divisors
              case (v: Var, Cst(len)) =>
                (2 to len-1).filter {
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
        var avoided = 0
        var failed = 0
        var crashed = 0
        var best_time = Double.PositiveInfinity
        var all_times: List[Double] = List.empty
        var best_substitutions = all_substitution_tables.head


        all_substitution_tables.foreach(st => {

          val tuned_expr = Utils.quickAndDirtySubstitution(st, tunableNodes.reverse, expr)
          TypeChecker(tuned_expr)
    
          //println(tuned_expr)
          //OpenCLGenerator.printTypes(tuned_expr)
          //System.exit(-1)

          counter = counter + 1
          //println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
          val (test, time) = executor(Math.min(best_time, 1000f), tuned_expr, values:_*)

          import ExecutionHarness.Status._
          test match {
            case Success =>
              passed = passed + 1
              all_times = time :: all_times
              if (time < best_time) {
                best_time = time
                best_substitutions = st
              }

            case Skipped =>
              skipped = skipped + 1

            case ValidationError =>
              println()
              println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
              println(tuned_expr)
              failed = failed + 1

            case Avoided =>
              avoided = avoided + 1

            case _ =>
              println()
              println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
              println(tuned_expr)
              crashed = crashed + 1
          }

          print(s"\r$counter / ${all_substitution_tables.size} ($passed passed, $skipped skipped, $avoided avoided, $failed failed, $crashed crashed) best = $best_time                   ")
        })

        println()
        println("All times:")
        println(all_times.mkString(", "))
        println(s"best time: ${best_time}")
        println("best parameters: " + best_substitutions.map(x => s"${x._1} -> ${x._2}").mkString("; "))
      }
    })


    // Step 4: Extract all variables from the constraint system and their dependencies
    //val parameters = findTuningKnobs(constraints)

    // Step 4: explore all possible values satisfying the constraints
    //val (localRange, globalRange) = InferNDRange(expr, values:_*)

    //executor(expr, globalRange, localRange, values:_*)

    // Cleanup
    Executor.shutdown()
  }



  def main (args: Array[String]) {

    // Start up the executor
    Executor.loadLibrary()
    Executor.init()

    // Prepare input and gold
    val mSize = AppParams.matrix_size
    val kSize = AppParams.matrix_size
    val nSize = AppParams.matrix_size
    println("Generating data")
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    println("Computing gold solution")
    val gold = Executor.nativeMatrixMultiply(
      matrixA.flatten.map(_.asInstanceOf[java.lang.Float]),
      matrixB.flatten.map(_.asInstanceOf[java.lang.Float]), mSize, nSize, kSize).map(_.toFloat)

    val executor = new ExecutionHarness(gold)
    val values = Seq(matrixA.transpose, matrixB)

    // Step 1: Get the expression for the re-writer:
    val expr = getHighLevelExpression()
    replaceInputTypes(expr)

    // Step 2: Find the tunable nodes in the expression
    val tunableNodes = Utils.findTunableNodes(expr)

    if(VERBOSE) println(s"Found ${tunableNodes.length} parameterizable nodes")

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

      println("Building substitution tables")

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

        val tuned_expr = Utils.quickAndDirtySubstitution(st, tunableNodes, expr)

        counter = counter + 1
        //println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
        val (test, time) = executor(Math.min(best_time, 1000f), tuned_expr, values:_*)

        import ExecutionHarness.Status._
        test match {
          case Success =>
            passed = passed + 1
            all_times = time :: all_times
            if (time < best_time) {
              best_time = time
              best_substitutions = st
            }

          case Skipped =>
            skipped = skipped + 1

          case ValidationError =>
            println()
            println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
            println(tuned_expr)
            failed = failed + 1

          case _ =>
            println()
            println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
            println(tuned_expr)
            crashed = crashed + 1
        }

        print(s"\r$counter / ${all_substitution_tables.size} ($passed passed, $skipped skipped, $failed failed, $crashed crashed) best = $best_time                   ")
      })

      println()
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

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")
//    val N = AppParams.matrix_size
//    val M = AppParams.matrix_size
//    val K = AppParams.matrix_size

    val f0 =
    /*      fun(
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
    */

      fun(
        ArrayType(ArrayType(Float, M), K),
        ArrayType(ArrayType(Float, N), K),
        (p54495403, p1260134048) =>
          FunCall(Join(),
            FunCall(MapWrg(1)(fun((p1408652377) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapWrg(0)(fun((p990416209) =>
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p1651855867) =>
                        FunCall(Scatter(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), p1651855867))),
                        FunCall(Join(),
                          FunCall(Map(fun((p1468303011) =>
                            FunCall(TransposeW(),
                              FunCall(Join(),
                                FunCall(Map(fun((p523691575) =>
                                  FunCall(TransposeW(), p523691575))), p1468303011))))),
                            FunCall(Map(fun((p1354011814) =>
                              FunCall(Map(fun((p1852584274) =>
                                FunCall(Map(fun((p1857815974) =>
                                  FunCall(TransposeW(), p1857815974))),
                                  FunCall(TransposeW(), p1852584274)))),
                                FunCall(TransposeW(), p1354011814)))),
                              FunCall(TransposeW(),
                                FunCall(toGlobal(fun((p520016214) =>
                                FunCall(MapSeq(fun((p1675763772) =>
                                  FunCall(MapLcl(1)(fun((p841283083) =>
                                    FunCall(MapLcl(0)(fun((p990398217) =>
                                    FunCall(MapSeq(fun((p1468357786) =>
                                      FunCall(MapSeq(fun((p36333492) =>
                                        FunCall(id, p36333492))), p1468357786))), p990398217))), p841283083))), p1675763772))), p520016214))),
                                  FunCall(ReduceSeq(fun((p1511785794, p527446182) =>
                          FunCall(fun((p1205555397) =>
                            FunCall(MapLcl(1)(fun((p1454031203) =>
                              FunCall(MapLcl(0)(fun((p407858146) =>
                                FunCall(Map(fun((p817406040) =>
                                  FunCall(Join(),
                                    FunCall(Transpose(), p817406040)))),
                                  FunCall(Transpose(),
                                    FunCall(ReduceSeq(fun((p603650290, p1754638213) =>
                                      FunCall(fun((p278934944) =>
                                        FunCall(MapSeq(fun((p222624801) =>
                                          FunCall(MapSeq(fun((p85777802) =>
                                            FunCall(add,
                                              FunCall(Get(0), p85777802),
                                              FunCall(Get(1), p85777802)))),
                                            FunCall(Zip(2),
                                              FunCall(Get(0), p222624801),
                                              FunCall(Get(1), p222624801))))),
                                          FunCall(Zip(2), p603650290,
                                            FunCall(MapSeq(fun((p280744458) =>
                                              FunCall(MapSeq(fun((p1213216872) =>
                                                FunCall(mult, p280744458, p1213216872))),
                                                FunCall(Get(1), p278934944)))),
                                              FunCall(toPrivate(fun((p1686369710) =>
                                                FunCall(MapSeq(fun((p385337537) =>
                                                  FunCall(id, p385337537))), p1686369710))),
                                                FunCall(Get(0), p278934944)))))),
                                        FunCall(fun((p1282811396) =>
                                          FunCall(Tuple(2),
                                            FunCall(Get(0), p1282811396),
                                          FunCall(toPrivate(fun((p439928219) =>
                                            FunCall(MapSeq(fun((p1883840933) =>
                                              FunCall(id, p1883840933))), p439928219))),
                                            FunCall(Get(1), p1282811396)))), p1754638213)))),
                                      FunCall(Get(0), p407858146),
                                      FunCall(Zip(2),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p1454031203)),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p407858146)))))))),
                                FunCall(Zip(2), FunCall(Get(0), p1454031203),
                                  FunCall(Split(workPerThreadMVar),
                                    FunCall(Gather(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)),
                                      FunCall(Transpose(),
                                        FunCall(Get(1), p1205555397)))))))),
                              FunCall(Zip(2), p1511785794,
                                FunCall(Split(workPerThreadNVar),
                                  FunCall(Transpose(),
                                    FunCall(Get(0), p1205555397)))))),
                            FunCall(fun((p1209669119) =>
                              FunCall(Unzip(),
                                FunCall(toLocal(fun((p1607305514) =>
                                  FunCall(MapLcl(1)(fun((p832279283) =>
                                    FunCall(Unzip(),
                                      FunCall(MapLcl(0)(fun((p668210649) =>
                                        FunCall(Tuple(2),
                                          FunCall(id,
                                            FunCall(Get(0), p668210649)),
                                          FunCall(id,
                                            FunCall(Get(1), p668210649))))),
                                        FunCall(Zip(2),
                                          FunCall(Get(0), p832279283),
                                          FunCall(Get(1), p832279283)))))), p1607305514))),
                                  FunCall(Zip(2),
                                    FunCall(Get(0), p1209669119),
                                    FunCall(Get(1), p1209669119))))), p527446182)))),
                                    FunCall(MapLcl(1)(fun((p1301664418) =>
                                      FunCall(MapLcl(0)(fun((p513169028) =>
                                        FunCall(MapSeq(fun((p377478451) =>
                                          FunCall(MapSeq(fun((p1596467899) =>
                                            FunCall(id, p1596467899))), p377478451))), p513169028))), p1301664418))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadMVar), workPerThreadNVar), tileSizeMNVar/^workPerThreadMVar), tileSizeMNVar/^workPerThreadNVar))),
                                    FunCall(Zip(2), p1408652377, p990416209))))))))))),
                    FunCall(Transpose(),
                      FunCall(Map(fun((p1952779858) =>
                        FunCall(Transpose(), p1952779858))),
                    FunCall(Split(tileSizeKVar),
                      FunCall(Map(fun((p1791868405) =>
                        FunCall(Split(tileSizeMNVar), p1791868405))), p1260134048))))))))),
              FunCall(Transpose(),
                FunCall(Map(fun((p81009902) =>
                  FunCall(Transpose(), p81009902))),
                  FunCall(Split(tileSizeKVar),
                    FunCall(Map(fun((p674483268) =>
                      FunCall(Split(tileSizeMNVar), p674483268))), p54495403)))))))

    TypeChecker(f0)
    f0
    // This is required to patch-up the address space modifiers for reduce
    //Lower.lowerNoAddressSpaces(f0)
  }

  def replaceInputTypes(lambda: Lambda): Unit = {
    val vars = lambda.params.flatMap(_.t.varList).distinct

    var st = ScalaImmMap[ArithExpr, ArithExpr]()

    vars.foreach(v => {
      st = st.updated(v, AppParams.matrix_size)
    })

    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
  }

  object ExecutionHarness {
    object Status extends Enumeration {
      type Status = Value
      val Success, Skipped, Avoided, ValidationError, ArithmeticsError, ExecutorError, UnknwownError = Value
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
    def apply(cur_best: Double, expr: Lambda, values: Any*): (Status, Double) = {
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

        // filter private memory
        val private_buffers_size = buffers.filter(_.mem.addressSpace == PrivateMemory)
        val private_alloc_size = private_buffers_size.map(_.mem.size).reduce(_+_).eval
        if(private_alloc_size > AppParams.max_amount_private_memory)
          return failure(Skipped)

        // filter local memory
        val local_buffers_size = buffers.filter(_.mem.addressSpace == LocalMemory)
        val local_alloc_size = 
          if(local_buffers_size.size != 0)   
            local_buffers_size.map(_.mem.size).reduce(_+_).eval
          else 0
        if(local_alloc_size > Executor.getDeviceLocalMemSize)
          return failure(Skipped)

        // Rule out obviously poor choices based on the grid size
        // - minimum of workitems in a workgroup
        if (local.map(_.eval).product < AppParams.min_work_items) return failure(Skipped)
        // - minimum size of the entire compute grid
        if (global.map(_.eval).product < AppParams.min_grid_size) return failure(Skipped)
        // - minimum number of workgroups
        val num_workgroups = (global.map(_.eval) zip local.map(_.eval)).map(x => x._1 / x._2).product
          println("num_workgroups = " + num_workgroups)
        if (num_workgroups < AppParams.min_num_workgroups || num_workgroups > AppParams.max_num_workgroups) {
          return failure(Skipped)
        }

        // This measures the % of max local memory / thread
        val resource_per_thread = if (local_alloc_size == 0) 0 else
          Executor.getDeviceLocalMemSize.toFloat /
            (Math.floor(Executor.getDeviceLocalMemSize / local_alloc_size) * local.map(_.eval).product.toFloat) /
            //                                                                   ^--- times # of work-items
            //                                                               ^--- # workgroup / sm
            //                                             ^--- usage per workgroup
            //              ^--- max local memory
            Executor.getDeviceLocalMemSize.toFloat * 100.0
        //  ^--- as a fraction of max mem            ^--- in %    

        // number of threads / SM
        if (resource_per_thread > AppParams.resource_per_thread) return failure(Skipped)

        // Avoid crashing for invalid values
        if(local.map(_.eval).product > Executor.getDeviceMaxWorkGroupSize)
          return failure(Skipped)

        // === Execution ===
        val (output: Array[Float], time) =
          Execute (local(0).eval, local(1).eval, global(0).eval, global(1).eval, (true, true) ) (10,cur_best*1.4f,expr, values: _*)

        if (false) {
          println()
          println("Current run:")
          println("- local variables: " + private_buffers_size.map(x => OpenCLMemory.getMaxSizeInBytes(x.t)).reduce(_+_).eval)
          println("- local size: " + local.map(_.eval).mkString(", "))
          println("- global size: " + global.map(_.eval).mkString(", "))
          println("- work item count: " + local.map(_.eval).product)
          println("- work group count: " + (global.map(_.eval) zip local.map(_.eval)).map(x => x._1 / x._2).product)
          println("- local allocation: " + local_alloc_size)
          println("- private allocation: " + private_alloc_size)
          println("- local resource / thread: " + resource_per_thread)
          println("- execution time: " + time)
        }

        if(time < 0) return failure(Avoided)

        // cross validation
        if (!AppParams.only_crossvalidate_better_solutions || 
            (AppParams.only_crossvalidate_better_solutions && time < cur_best)) {
          if (output.length != gold.length)
            failure(ValidationError)
          else {
            val passed = (output zip gold).forall(x => Math.abs(x._1 - x._2) < 0.01f * Math.max(Math.abs(x._1), Math.abs(x._2)))
            if (passed) success(time)
            else {
              val mismatch = (output zip gold).collect{
                case x if Math.abs(x._1 - x._2) >= 0.01f * Math.max(Math.abs(x._1), Math.abs(x._2)) => x }.toList
              println("Error: " + mismatch.size + " / " + gold.size + " mismatch")
              println("Avg Error: " + mismatch.map(x => Math.max(Math.abs(x._1), Math.abs(x._2))/Math.min(Math.abs(x._1), Math.abs(x._2))-1).reduce(_+_)/mismatch.size*100.0f)
              println("Local size: " + local.mkString(", "))
              println("Global size: " + global.mkString(", "))
              println("Execution time: " + time)
              failure(ValidationError)
            }
          }
        } else success(time)
      } catch {
        case ea: Executor.ExecutorFailureException =>
          ea.printStackTrace()
          ea.consume()
          failure(ExecutorError)
        case e: Exception =>
          e.printStackTrace()
          failure(UnknwownError)
      }
    }
  }
}
