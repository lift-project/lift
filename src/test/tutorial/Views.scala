package tutorial

import ir._
import ir.ast._
import ir.printer.DotPrinter
import lift.arithmetic._
import opencl.executor.{Compile, Execute}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Test


class Views{
  /**
    * In the Lift IR, arrays are not accessed explicitly but implicitly;
    * the patterns determine which thread accesses which element in memory.
    * This design simplifies the process of lowering high-level programs to
    * the Lift IR and guarantees that data races are avoided by construction
    * since no arbitrary accesses into memory are permitted. However, this
    * introduces two main challenges when compiling the Lift IR:
    *
    * First, avoiding unnecessary intermediate results arising from function which
    * change  only  the  data  layout;
    *
    * And,  secondly,  generating efficient accesses to multi-dimensional arrays
    * which have a flat representation in memory.
    */

  /** Simple example: A one-dimensional 3-point stencil computation.
    * For each value of the input array, we consider the left neighbor, the current
    * element and the right neighbor and sum them up to compute a new output element.
    * For the elements at the boundaries, which lack either the left or right neighbor,
    * we specify a boundary handling which determines which elements to use instead.
    * In this case we use the clamping boundary condition which repeats the outermost
    * elements
    */
  @Test
  def stencil1D(): Unit = {
    val N = SizeVar("N")
    val clamp = Pad.Boundary.Clamp

    /**
      * We start with defining the high-level expression which solely expresses
      * *what* to compute
      */

    val highLevel = fun(
      ArrayType(Float, N), input =>
        Map(Reduce(add, 0.0f)) o
          Slide(3,1) o
            Pad(1,1,clamp) $ input
    )

    /**
      * Since we cannot compile the high-level expression to OpenCL code yet,
      * we introduce low-level patterns which specify how to map the computation
      * to OpenCL's thread and memory hierarchy.
      *
      * Here, we want to process each neighborhood by a single thread indicated
      * by MapGlb
      */

    val lowLevel = fun(
      ArrayType(Float, N), input =>
        MapGlb(MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f)) o
          Slide(3,1) o
            Pad(1,1,clamp) $ input
    )

    /**
      * This code can now be compiled to OpenCL.
      */
    val kernel = Compile(lowLevel)
    println(kernel)
    /**
      * Two for loops have been generated, one for MapGlb and one for ReduceSeq.
      * Applying Pad and Slide to the input did not create temporary results.
      * Instead, they influence how to read from the input when computing the reduction.
      *
      * In Lift, primitives like Pad and Slide, which only modify the data layout,
      * create Views which reflect the modification of the data layout rather than
      * creating temporary results.
      *
      * Let's look at a small example: Let A = [a,b,c,d] a one-dimensional array
      * containing 4 elements.
      */
    val A = Array(1.0f, 2.0f, 3.0f, 4.0f)

    /**
      * Conceptually, applying Pad(1,1,clamp) $ A results in the following array:
      * A' = [a,a,b,c,d,d]
      *
      * However, this array A' will not be created in memory. Instead Pad now influences
      * how subsequent patterns read from A.
      * Say we have a simple computation like:
      */
    val simpleComputation = λ(
      ArrayType(Float, N), A =>
        MapGlb(square) o Pad(1,1,clamp) $ A
    )
    /**
      * We generate a for-loop for the MapGlb which accesses A rather than A':
      *
      * for(int gid = get_global_id(0); gid < N+2; gid += get_global_size(0) {
      *   OUT[gid] = IN[ ??? ];
      * }
      *
      * The for-loop goes from 0 to N+1 because Pad 'extended' the arrays with two
      * elements, one left, and one right. How to access IN is now defined in the
      * View created by Pad which given an index i' into the conceptual array A' returns
      * an index i into A. (specified in emitView() )
      *
      * So the index looks something like IN[ padView(i') ]
      * For the clamping boundary handling, the padView function looks like:
      *
      * (i'-1) >= 0 ? ((i'-1) < N ? i'-1 : N-1) : 0
      *
      * making sure that every index is in bounds of A' and if not, repeating the
      * outermost element.
      */
    println(Compile(simpleComputation))

    /**
      * Primitives like Slide or Split increase the dimension of their input arrays.
      */
    val simpleComputation2 = λ(
      ArrayType(Float, N), B =>
        MapGlb(MapSeq(square)) o Slide(3,1) o Pad(1,1,clamp) $ B
    )
    /**
      * This generated code similar to this:
      * for(int gid = get_global_id(0); gid < N; gid += get_global_size(0) {
      *   for(int i = 0; i < 3; i++ {
      *     OUT[gid] = IN[ ??? ]
      *   }
      * }
      *
      * The two Maps generated two for-loops. These two indices (gid, i) need to be
      * flattened in order to access our one-dimensional input B.
      *
      * The SlideView explains how to map a two-dimensional access (i',j') to a one-dimensional
      * index i. In this case two views (the SlideView and PadView) are nested which produces something like
      * IN[ padView( slideView(gid, i) ) ];
      *
      * where the slideView explains how to flatten the two-dimensional access and the padView explains
      * how to compute the index as described above.
      *
      * In this way, the Views of data-layout primitives like Pad and Slide are nested until they are
      * 'consumed' by a primitives which actually requires computation like Map or Reduce
      * containing a UserFun like square
      */
    println(Compile(simpleComputation2))


    val lowLevelUnrolled = λ(
      ArrayType(Float, N), input =>
        MapGlb(MapSeq(toGlobal(id)) o ReduceSeqUnroll(add, 0.0f)) o
          Slide(3,1) o
            Pad(1,1,clamp) $ input
    )


    DotPrinter(System.getProperty("java.io.tmpdir"), "expr", lowLevelUnrolled)
  }

  @Test
  def stencil9pt() : Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val clamp = Pad.Boundary.Clamp // repeat elements on the borders of the input

    /** We start with defining the high-level expression which solely expresses
      * *what* to compute
      */
    val highLevelExpression = λ(
      ArrayType(ArrayType(Float, M), N), input => // given a 2D input..

      Map(Map(                      // (3.1) for each 9pt neighborhood:
        Reduce(add, 0.0f) o Join()  // (3.2) flatten and reduce neighborhoods
      )) o Slide2D(3,1) o           // (2) create 9pt neighborhoods
        Pad2D(1,1,clamp) $ input    // (1) 2D boundary handling
    )

    /**
      * Since we cannot compile the high-level expression to OpenCL code yet,
      * we introduce low-level patterns which specify how to map the computation
      * to OpenCL's thread and memory hierarchy.
      *
      * Here, we want to process each neighborhood by a single thread, using
      * threads in the first (x) and second (y) dimension.
      *
      * We also specify that the reduction of the 9 neighborhood elements
      * shall be unrolled in the OpenCL code using ReduceSeqUnroll.
      */

    val lowLevelExpression = fun(
      ArrayType(ArrayType(Float, M), N), input =>

      MapGlb(1)(MapGlb(0)(
        MapSeq(toGlobal(id)) o ReduceSeqUnroll(add, 0.0f) o Join()
      )) o Slide2D(3,1) o
        Pad2D(1,1,clamp) $ input
    )

    /**
      * This expression can now be compiled to OpenCL code.
      */
    val kernel = Compile(lowLevelExpression)
    println(kernel)
    // set LIFT_NO_ARITH_SIMPL = 1
  }

  @Test
  def arithExprSimpl(): Unit = {
    // 1 + N - 1
    val example1 = Cst(1) + Var("N") - Cst(1)
    //println(example1)

    // N > 0 ? T : F
    val example2 = (SizeVar("N") ge Cst(0)) ?? Var("T") !! Var("F")
    println(example2)

    // ((2*M) + 1) % M
    val M = Var("M")
    val example3 = ((2 * M) + 1) % M
    //println(example3)
  }


}
