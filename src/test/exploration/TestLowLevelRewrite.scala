package exploration

import apart.arithmetic.{Prod, Cst, ArithExpr, Var}
import ir.ArrayType
import ir.ast._
import opencl.executor.{Executor, Execute}
import opencl.generator.OpenCLGenerator.NDRange
import opencl.ir._
import opencl.ir.pattern.{ReduceSeq, MapSeq}
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
    val mSize = 256
    val kSize = 256
    val nSize = 256
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)
    val executor = new ExecutionHarness(gold.flatten)
    val values = Seq(matrixA.transpose, matrixB)

    // Step 1: Get the expression for the re-writer:
    val expr = getHighLevelExpression()

    // Step 2: Find the tunable nodes in the expression
    val tunableNodes = findTunableNodes(expr)

    // Step 3: find constraints across the nodes
    val constraints = FindParameterConstraints(tunableNodes)
    
    // Step 4: Extract all variables from the constraint system and their dependencies
    val parameters = findTuningKnobs(constraints)

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
    val N = 256
    val M = 256
    val K = 256

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

  /** Quick execution harness to restart the executor on failure. */
  class ExecutionHarness(gold: Array[Float]) {

    // Point of failure enum to trace crash
    object PoF extends Enumeration {
      type PoF = Value
      val ValidationError, ArithmeticsError, ExecutorError, UnknwownError = Value
    }
    import PoF._

    // best execution time so far
    var best_time = scala.Float.PositiveInfinity

    // Function called on successful execution and cross validation
    def success(time: Double): Boolean = {
      println("Passed")
      true
    }

    // Error callback
    def failure(reason: PoF): Boolean = {
      println("Failed")
      false
    }

    // run the given lambda with the given dimensions and parameters
    def apply(expr: Lambda, global: NDRange, local: NDRange, values: Any*): Boolean = {
      try {
        // execution
        val (output: Array[Float], time) =
          Execute (local(0).eval, local(1).eval, global(0).eval, global(1).eval, (true, true) ) (expr, values: _*)

        // cross validation
        if (output.length != gold.length) failure(ValidationError)
        else if ((output zip gold).forall(x => (x._1 - x._2).abs < 0.001f)) success(time)
        else failure(ValidationError)
      } catch {
        case ea: Executor.ExecutorFailureException =>
          ea.consume()
          failure(ExecutorError)
        case e: Exception => failure(UnknwownError)
      }
    }
  }
}
