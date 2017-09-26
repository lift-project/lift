package opencl.generator.stencil

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.{Compile, Execute, _}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Test
import rewriting.rules.{FusionRules, Rules}
import rewriting.Rewrite
import rewriting.macrorules.{EnablingRules, MacroRules}

object GettingStarted extends TestWithExecutor

class GettingStarted {
  /**
    * In the following, we introduce how to write simple stencil
    * computations in Lift.
    *
    * Stencil applications are a class of algorithms which update
    * elements in a multi dimensional grid based on the neighboring
    * values using a fixed pattern — the stencil. They are used
    * extensively in various domains such as medical imaging (e.g.,
    * SRAD), scientific workloads (e.g., fluid simulation), numerical
    * methods (e.g., Jacobi) or machine-learning (e.g., convolutional
    * neural network).
    *
    * Consider a 1D 3-point stencil applied on a one-dimensional
    * array A of length N that simply sums up the elements of each
    * neighborhood. A simple C program implementing this stencil is
    * shown in the following:
    */

  /* Listing 1: 3pointJacobi.c

    for(int i = 0; i < N; i++) {
      int sum = 0;
      for(int j = -1; j <= 1; j++) {  // (a)
        int pos = i+j;
        pos = pos < 0 ? 0 : pos;      // (b)
        pos = pos > N-1 ? N-1 : pos;
        sum += A[pos];                // (c)
        }
     B[i] = sum;
   }
   */

  /**
    * Instead of expressing stencil computations using a single
    * high-level stencil primitive, as often seen in other high-level
    * approaches, in LIFT, we aim for composability and, therefore,
    * express stencil computations using smaller fundamental building
    * blocks.
    *
    * As denoted in the comments, stencil computations consist of three
    * fundamental parts:
    *
    * (a) for every element of the input, a neighborhood is accessed
    * specified by the shape of the stencil
    *
    * (b) boundary handling is performed which specifies how to
    * handle neighboring values for elements at the borders of
    * the input grid
    *
    * (c) finally, for each neighborhood their elements are used to
    * compute an output element
    *
    * In Lift, we express the stencil computation shown in Listing 1
    * as follows:
    */

  val clamp = Pad.Boundary.Clamp // abbreviation
  def simple3pointJacobi =
    Map(Reduce(add, 0.0f)) o              // (c)
      Slide(3, 1) o                       // (a)
        Pad(1, 1, clamp)                  // (b)

  /**
    * Given an array A = [a,b,c,d] this expression is evaluated as follows:
    *
    *   Map(Reduce(add,0.0f)) o Slide(3,1) o Pad(1,1,clamp) $ [a,b,c,d]
    * = Map(Reduce(add,0.0f)) o Slide(3,1) $ [a,a,b,c,d,d]
    * = Map(Reduce(add,0.0f)) $ [ [a,a,b],[a,b,c],[b,c,d],[c,d,d] ]
    * = [ [a+a+b],[a+b+c],[b+c+d],[c+d+d] ]
    *
    *
    * High-level expressions solely describe 'what' is being computed.
    * Low-level expressions additionally specify 'how' the computation is
    * executed. Therefore, Lift's low-level Map primitives explicitly exploit
    * OpenCL's thread hierarchy (MapGlb, MapWrg, MapSeq, ...)
    * and OpenCL's memory hierarchy (toGlobal, toLocal, toPrivate).
    *
    * In order to execute this we need to transform this high-level
    * expression to a low-level expression which then is compiled to an
    * executable OpenCL kernel (High-Level => Low-Level => OpenCL):
    */

  def lowLevel3pointJacobi =
    λ(ArrayTypeWSWC(Float, SizeVar("N")),
      A =>
        /**
          * The high-level Map primitive has been replaced with the low-level
          * MapSeq (long: MapSequential) primitive. This primitive states
          * the the given function shall be applied sequentially to the given
          * array
          */
      MapSeq(                                               // (c)
        /**
          * The toGlobal primitives ensures that the final result resides in
          * global memory as required by OpenCL.
          * ReduceSeq explicitly states that the reduction is executed
          * sequentially as well
          */
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)
      ) o
        Slide(3, 1) o                                       // (a)
          Pad(1, 1, clamp) $ A                              // (b)
    )

  /** lets test it... */
  @Test def firstStencilTest(): Unit = {
    val input = Array.tabulate(4) { i => i * 1.0f }
    println(s"Input:\t [${input.mkString(", ")}]")

    /**
      * 'Execute' type-checks the lambda, compiles it to OpenCL and executes
      * it using the given input
      */
    val (output, _) = Execute()[Array[Float]](lowLevel3pointJacobi, input)
    println(s"Output:\t [${output.mkString(", ")}]")
  }

  /************************************************************************
    * EXPLORATION                                                         *
  /***********************************************************************/
    *
    * During exploration, we transform a high-level expression by
    * applying rewrite rules of the from
    *
    *   lhs == rhs
    *
    * A rewrite rule states that in an arbitrary expression, lhs always
    * can be replaced with rhs without changing the semantics. In Lift,
    * we encode common optimizations as rewrite rules and transform our
    * expression using an automated rewrite system.
    *
    * Implementation note: Rules are implemented in Lift as (lhs => rhs)
    * and (rhs => lhs)
    * e.g., Map(f) o Map(g) => Map(f o g) // mapFusion
    * and   Map(f o g) => Map(f) o Map(g) // mapFission
    *
    * The mapFusion rule is a simple example for such a rewrite rule:
    */

  def f = λ(x => x)
  def g = \(x => x) // equivalent to f using different syntax
  def lhs = Map(f) o Map(g)
  def rhs = Map(f o g)

  /**
    * The slideTiling rule is another example used for rewriting
    * stencil computations:
    */

  // neighbourhood parameters
  val n = Var("n")
  val s = Var("s")

  // tile parameters
  val u = Var("u")
  val v = Var("v")

  def lhs_slideTiling = Slide(n,s)
  def rhs_slideTiling = Join() o Map(Slide(n,s)) o Slide(u,v)

  /** another rule: movingJoin */
  def lhs_mapPromotion = Map(f) o Join()
  def rhs_mapPromotion = Join() o Map(Map(f))

  /**
    * These rules allow us to rewrite our stencil expression.
    * In the following, we manually transform our simpleStencil
    * expression to an expression which applies overlapped tiling -
    * a common optimization for stencil computations using only the
    * previously defined rules:
    */
  def simpleStencil = λ(ArrayTypeWSWC(Float, SizeVar("N")), A =>
    Map(Reduce(add, 0.0f)) o
      Slide(3,1) o                          // <- apply slideTiling here
        Pad(1,1,clamp)
  $ A)

  def rewrite1 = Map(Reduce(add, 0.0f)) o  // <- move Map over Join
    Join() o Map(Slide(3,1)) o Slide(u,v) o
      Pad(1,1,clamp)

  def rewrite2 = Join() o Map(Map(Reduce(add, 0.0f))) o // <- fuse outer Map
    Map(Slide(3,1)) o Slide(u,v) o                      // <- with this Map
      Pad(1,1,clamp)

  def tiledStencil = Join() o Map(Map(Reduce(add, 0.0f)) o
    Slide(3,1)) o Slide(u,v) o Pad(1,1,clamp)

  /**
    * This is how it is implemented in Lift:
    */
  @Test def applyRewritesInLift = {
    val slideTiling = Rewrite.applyRuleAtId(simpleStencil, 1, Rules.slideTiling)
    val promotedMap = Rewrite.applyRuleAtId(slideTiling, 0, EnablingRules.movingJoin)
    val fusedMaps = Rewrite.applyRuleAtId(promotedMap, 1, FusionRules.mapFusion)

    // print Lifts internal representation of a λ-function
    println(fusedMaps)
  }

  /**
    * Exploration is divided into three steps:
    *
    * 1) HighLevelRewrite
    * 2) MemoryMappingRewrite
    * 3) ParameterRewrite
    *
    * HighLevelRewrite applies rewrite rules as shown above to transform a
    * a single high-level expression into multiple different expressions.
    *
    * In order to generate OpenCL code, we need to lower this expression
    * into a low-level Lift expression using Lift's low-level primitives.
    * This happens during MemoryMappingRewrite.
    *
    * ParameterRewrite finally replaces all numerical parameters (such as
    * tile sizes) with concrete values
    *
    *
    * In the following we manually lower the tiledStencil expression.
    * Specifically we assign work to OpenCL's work-items by using MapGlb
    * and specify that the result needs to be copied to global memory at the
    * end of the computation using toGlobal:
    */

  def tiled3pointJacobi = λ(
    ArrayTypeWSWC(Float, SizeVar("N")),
    A =>
    Join() o MapGlb(1)(MapGlb(0)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
      Map(Slide(3, 1)) o Slide(4, 2) o
        Pad(1,1,clamp) $ A
  )

  /**
    * Let's check if it still computes the same result and have a look at the
    * generated kernel by setting LIFT_VERBOSE=1
    */
  @Test def secondStencilTest(): Unit = {
    val input = Array.tabulate(4) { i => i * 1.0f }

    /**
      * Here, we specify that we want to start the kernels using a 2D localsize (1,1) and
      * a 2D globalsize (2,2). Thus, we explicitly start 4 OpenCL threads in total.
      *
      * (false,false) specifies that we don't want to inject this information into kernel generation.
      * Therefore the kernel is still executable with arbitrary global- and localsizes and not
      * specialized for this specific combination
      */
    val (gold, _) = Execute(1,1,2,2,(false,false))[Array[Float]](lowLevel3pointJacobi, input)
    val (output, _) = Execute(1,1,2,2,(false,false))[Array[Float]](tiled3pointJacobi, input)

    assertArrayEquals(gold, output, 0.001f)
  }

  /**
    * The following kernel contains a var as tile size instead of a concrete
    * numeric parameter
    */
  @Test def autotuningStencils(): Unit = {
    val tuningParam = Var("param1") // tileStep
    def almostTunableStencil = λ(
      ArrayTypeWSWC(Float, SizeVar("N")),
      A =>
        Join() o MapWrg(MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
          Map(Slide(3, 1)) o Slide(tuningParam + 2, tuningParam) o
          Pad(1, 1, clamp) $ A
    )

    val kernel = Compile(almostTunableStencil)
    println(kernel)

    // currently not possible
    //val input = Array.tabulate(1024) { i => i * 1.0f }
    //val (output: Array[Float], _) = Execute(32, 1024, (false, false))(almostTunableStencil, input)
  }
}
