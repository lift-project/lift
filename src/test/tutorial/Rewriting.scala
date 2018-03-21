package tutorial

import ir._
import ir.ast._
import ir.printer.DotPrinter
import lift.arithmetic._
import opencl.executor.{Compile, Execute}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Test
import org.junit.Assert._
import rewriting.Rewrite
import rewriting.macrorules.EnablingRules
import rewriting.rules.{FusionRules, OpenCLRules, Rules}


class Rewriting{
   /**
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
  val clamp = Pad.Boundary.Clamp
  def simpleStencil = λ(ArrayType(Float, SizeVar("N")), A =>
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

  def localMemoryJacobi = λ(
    ArrayTypeWSWC(Float, SizeVar("N")),
    A =>
      Join() o MapWrg(
        MapLcl(

        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f)) o Slide(3,1) o
            MapLcl(toLocal(id))) o

        Slide(4,2) o
        Pad(1,1,clamp) $ A
  )
  println(Compile(localMemoryJacobi))

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
  @Test
  def simple(): Unit = {
    val N = SizeVar("N")

    val expression1 = λ(
      ArrayType(Float, N), input =>
        Map(Reduce(add, 0.0f)) o
          Slide(3, 1) o
          Pad(1, 1, clamp) $ input
    )

    val f = Reduce(add, 0.0f)
    val expression2 = λ(
      ArrayType(Float, N), input =>
        Map(f) o Slide(3, 1) o
          Pad(1, 1, clamp) $ input
    )

    //DotPrinter("/home/bastian/presentations/ispass2018/slides/img", "expression2dot", expression2)

    val expression31 = λ(
      ArrayType(Float, N), input =>
        Join() o
          Map(Map(f) o Slide(3, 1)) o
            Slide(u, v) o
              Pad(1, 1, clamp) $ input
    )

    val expression3 = λ(
      ArrayType(Float, N), input =>
        Join() o
          Map(λ(tile =>
            Map(f) o Slide(3, 1) $ tile)) o
            Slide(u, v) o
              Pad(1, 1, clamp) $ input
    )

    val expression4 = λ(
      ArrayType(Float, N), input =>
        Join() o
          MapWrg(λ(tile =>
            MapLcl(f) o Slide(3, 1) $ tile)) o
            Slide(u, v) o
              Pad(1, 1, clamp) $ input
    )

    println(expression4)
    val expression41 = Rewrite.applyRuleAtId(
      expression3, 1, OpenCLRules.mapWrg)

    val localMemoryJacobi = λ(
    ArrayType(Float, SizeVar("N")),
    A =>
      Join() o MapWrg(
        MapLcl(

        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f)) o Slide(3,1) o
            MapLcl(toLocal(id))) o

        Slide(4,2) o
        Pad(1,1,clamp) $ A
  )

    val f0 = Rewrite.applyRuleAtId(expression2, 1, Rules.slideTiling)
    val f1 = Rewrite.applyRuleAtId(f0, 0, EnablingRules.movingJoin)
    //DotPrinter.withNumbering("/home/bastian/", "tiled", expression3)

    DotPrinter.withNumbering("/home/bastian/", "tiled", localMemoryJacobi)
    /*
    val tiled = Rewrite.applyRuleAt(funCall, Rules.slideTiling, slideCall)
        val moved = Rewrite.applyRuleAt(tiled, EnablingRules.movingJoin, tiled)
        val fused = Rewrite.applyRuleAtId(moved, 1, )
        */
    println(expression1)
    println(expression2)
    println(expression3)

  }

}
