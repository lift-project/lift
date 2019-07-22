package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestPad extends TestWithExecutor

class TestPad {
  import ir.ast.Pad.Boundary._

  // use letters instead of numbers
  val next = { var n = -1f; () => { n = n + 1f; n } }
  val a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p = next()
  // same thing with uppercase to distinguish padding
  val Next = { var n = -1f; () => { n = n + 1f; n } }
  val A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P  = Next()

  /// Input array
  val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
  val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}
  val input3D = Array.tabulate(2, 2, 2) { (i, j, k) => i * 4.0f + j * 2.0f + k}

  def validate1D(gold: Array[Float], left: Int, right: Int, boundary: Pad.BoundaryFun, input: Array[Float] = input): Unit = {
    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id) o Pad(left, right, boundary) $ domain
    )

    val (output,runtime) = Execute(input.length, input.length)[Array[Float]](fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  /* **********************************************************
        PAD 1D
     ***********************************************************/
  @Test def padAfterPad(): Unit = {
    val gold = Array(A,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,P)
    val bf = Pad.Boundary.Clamp
    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id) o Pad(1,1,bf) o Pad(1,1,bf) $ domain
    )

    val (output, _) = Execute(input.length, input.length)[Array[Float]](fct, input)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def padMapPad(): Unit = {
    val input = Array(0,1,2,3,4).map(_.toFloat)
    val gold = Array(0,0,0,1,2,3,4,4,4).map(_.toFloat)
    val bf = Pad.Boundary.Clamp
    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => PrintType() o MapSeq(id) o Pad(1,1,bf) o MapSeq(id) o Pad(1,1,bf) $ domain
    )

    val (output, _) = Execute(5,5)[Array[Float]](fct, input)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Ignore // needs to fail! todo change to check for failure
  @Test def joinMapPadRewrite(): Unit = {
    val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}
    val bf = Pad.Boundary.Clamp
    val joinMapPad = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")),
      (domain) => MapSeq(id) o Join() o MapSeq(MapSeq(id)) o Pad(1,1,bf) $ domain
    )
    val padJoinMap = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")),
      (domain) => MapSeq(id) o Pad(1,1,bf) o Join() o MapSeq(MapSeq(id)) $ domain
    )

    val (output1, _) = Execute(128,128)[Array[Float]](joinMapPad, input2D)
    val (output2, _) = Execute(128,128)[Array[Float]](padJoinMap, input2D)
    assertArrayEquals(output1, output2, 0.0f)
  }

  @Test def pad1DClampSize1(): Unit = {
    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    println(gold.mkString(", "))
    validate1D(gold, 1,1, Clamp)
  }

  @Test def pad1DClampSize2(): Unit = {
    val gold = Array(A,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,P)
    validate1D(gold, 2,2, Clamp)
  }

  @Test def pad1DClampLeft2(): Unit = {
    val gold = Array(A,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    validate1D(gold, 2,0, Clamp)
  }

  @Test def pad1DClampRight2(): Unit = {
    val gold = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,P)
    validate1D(gold, 0,2, Clamp)
  }

  @Test def pad1DClampSize8(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(A,A,A,A,A,A,A,A,a,b,c,d,D,D,D,D,D,D,D,D)

    validate1D(gold, 8,8, Clamp, input)
  }

  @Test def pad1DMirrorSize1(): Unit = {
    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    validate1D(gold, 1,1, Mirror)
  }

  @Test def pad1DMirrorSize2(): Unit = {
    val gold = Array(B,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,O)
    validate1D(gold, 2,2, Mirror)
  }

  @Test def pad1DMirrorSize8(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(A,B,C,D,D,C,B,A,a,b,c,d,D,C,B,A,A,B,C,D)

    validate1D(gold, 8,8, Mirror, input)
  }

  @Test def pad1DMirrorUnsafeSize1(): Unit = {
    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    validate1D(gold, 1,1, MirrorUnsafe)
  }

  @Test def pad1DMirrorUnsafeSize2(): Unit = {
    val gold = Array(B,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,O)
    validate1D(gold, 2,2, MirrorUnsafe)
  }

  @Test def pad1DMirrorUnsafeSize4(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(D,C,B,A,a,b,c,d,D,C,B,A)

    validate1D(gold, 4,4, MirrorUnsafe, input)
  }

  @Test def pad1DWrapSize1(): Unit = {
    val gold = Array(P,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,A)
    validate1D(gold, 1,1, Wrap)
  }

  @Test def pad1DWrapSize2(): Unit = {
    val gold = Array(O,P,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,A,B)
    validate1D(gold, 2,2, Wrap)
  }

  @Test def pad1DWrapSize8(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(A,B,C,D,A,B,C,D,a,b,c,d,A,B,C,D,A,B,C,D)

    validate1D(gold, 8,8, Wrap, input)
  }

  /* **********************************************************
        PAD 2D
     ***********************************************************/
  def validate2D(gold: Array[Float],
                 top: Int, bottom: Int,
                 left: Int, right: Int,
                 boundary: Pad.BoundaryFun): Unit = {
    val N = SizeVar("N")
    val fct = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (domain) => MapGlb(0)(
        MapGlb(1)(id)) o
        Pad2D(top, bottom, left, right, boundary) $ domain
    )

    val (output,runtime) = Execute(gold.length,gold.length)[Array[Float]](fct, input2D)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def pad2DClampSize1(): Unit = {
    val gold = Array(
      A, A, B, C, D, D,
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P)

    validate2D(gold, 1,1,1,1, Clamp)
  }
  @Test def pad2DClampLeftOnly(): Unit = {
    val gold = Array(
      A, a, b, c, d,
      E, e, f, g, h,
      I, i, j, k, l,
      M, m, n, o, p)

    validate2D(gold, 0,0,1,0, Clamp)
  }

  @Test def pad2DRightOnly(): Unit = {
    val gold = Array(
      a, b, c, d, D,
      e, f, g, h, H,
      i, j, k, l, L,
      m, n, o, p, P)

    validate2D(gold, 0,0,0,1, Clamp)
  }

  @Test def pad2DClampBottomOnly(): Unit = {
    val gold = Array(
      a, b, c, d,
      e, f, g, h,
      i, j, k, l,
      m, n, o, p,
      M, N, O, P)

    validate2D(gold, 0,1,0,0, Clamp)
  }

  @Test def pad2DClampSizeTopOnly(): Unit = {
    val gold = Array(
      A, B, C, D,
      a, b, c, d,
      e, f, g, h,
      i, j, k, l,
      m, n, o, p)

    validate2D(gold, 1,0,0,0, Clamp)
  }

  @Test def pad2DClampSize2(): Unit = {
    val gold = Array(
      A, A, A, B, C, D, D, D,
      A, A, A, B, C, D, D, D,
      A, A, a, b, c, d, D, D,
      E, E, e, f, g, h, H, H,
      I, I, i, j, k, l, L, L,
      M, M, m, n, o, p, P, P,
      M, M, M, N, O, P, P, P,
      M, M, M, N, O, P, P, P)

    validate2D(gold, 2,2,2,2, Clamp)
  }

  @Test def pad2DMirrorSize1(): Unit = {
    val gold = Array(
      A, A, B, C, D, D,
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P)

    validate2D(gold, 1,1,1,1, Mirror)
  }

  @Test def pad2DMirrorSize2(): Unit = {
    val gold = Array(
      F, E, E, F, G, H, H, G,
      B, A, A, B, C, D, D, C,
      B, A, a, b, c, d, D, C,
      F, E, e, f, g, h, H, G,
      J, I, i, j, k, l, L, K,
      N, M, m, n, o, p, P, O,
      N, M, M, N, O, P, P, O,
      J, I, I, J, K, L, L, K)

    validate2D(gold, 2,2,2,2, Mirror)
  }

  @Test def pad2DMirrorUnsafeSize1(): Unit = {
    val gold = Array(
      A, A, B, C, D, D,
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P)

    validate2D(gold, 1,1,1,1, MirrorUnsafe)
  }

  @Test def pad2DMirrorUnsafeSize2(): Unit = {
    val gold = Array(
      F, E, E, F, G, H, H, G,
      B, A, A, B, C, D, D, C,
      B, A, a, b, c, d, D, C,
      F, E, e, f, g, h, H, G,
      J, I, i, j, k, l, L, K,
      N, M, m, n, o, p, P, O,
      N, M, M, N, O, P, P, O,
      J, I, I, J, K, L, L, K)

    validate2D(gold, 2,2,2,2, MirrorUnsafe)
  }

  @Test def pad2DWrapSize1(): Unit = {
    val gold = Array(
      P, M, N, O, P, M,
      D, a, b, c, d, A,
      H, e, f, g, h, E,
      L, i, j, k, l, I,
      P, m, n, o, p, M,
      D, A, B, C, D, A)

    validate2D(gold, 1,1,1,1, Wrap)
  }

  @Test def pad2DWrapSize2(): Unit = {
    val gold = Array(
      K, L, I, J, K, L, I, J,
      O, P, M, N, O, P, M, N,
      C, D, a, b, c, d, A, B,
      G, H, e, f, g, h, E, F,
      K, L, i, j, k, l, I, J,
      O, P, m, n, o, p, M, N,
      C, D, A, B, C, D, A, B,
      G, H, E, F, G, H, E, F)

    validate2D(gold, 2,2,2,2, Wrap)
  }
  /* **********************************************************
      THESIS
  ***********************************************************/
  @Test def reducePad(): Unit = {
    val input = Array(0,1,2,3,4,5).map(_.toFloat)
    val gold = Array(20.0f)
    val bf = Pad.Boundary.Wrap
    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Pad(1,1,bf) $ input
    )

    val (output, _) = Execute(1,1)[Array[Float]](fct, input)
    println(output.mkString(", "))
    println("test")
    //assertArrayEquals(gold, output, 0.0f)
  }

  /* **********************************************************
        PAD 3D
     ***********************************************************/
  def validate3D(gold: Array[Float],
                 z: Int,
                 y: Int,
                 x: Int,
                 b: Pad.BoundaryFun): Unit = {
    val N = SizeVar("N")
    val fct = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      (domain) => MapGlb(0)(MapGlb(1)(MapGlb(2)(id))) o
        Pad3D(z,y,x,b) $ domain
    )

    val (output,runtime) = Execute(gold.length,gold.length)[Array[Float]](fct, input3D)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  val gold3d = Array(A,A,B,B, // boundary = clamp
    A,A,B,B,
    C,C,D,D,
    C,C,D,D,
    A,A,B,B,
    A,a,b,B,
    C,c,d,D,
    C,C,D,D,
    E,E,F,F,
    E,e,f,F,
    G,g,h,H,
    G,G,H,H,
    E,E,F,F,
    E,E,F,F,
    G,G,H,H,
    G,G,H,H)

  @Test def pad3d(): Unit = {
    println(input3D.transpose.flatten.flatten.mkString(","))
    validate3D(gold3d,1,1,1,Pad.Boundary.Clamp)
  }

  @Test def pad3dAlternative(): Unit = {
    val N = SizeVar("N")
    val b = Pad.Boundary.Clamp
    val fct = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      (input) => MapGlb(0)(MapGlb(1)(MapGlb(2)(id))) o
        Map(Map(Pad(1,1,b))) o Transpose() o
        Pad(1,1,b) o Transpose() o Pad(1,1,b) $ input
    )

    val (output,runtime) = Execute(gold3d.length,gold3d.length)[Array[Float]](fct, input3D)
    println("runtime = " + runtime)
    assertArrayEquals(gold3d, output, 0.0f)
    println(output.mkString(","))
  }

  /* **********************************************************
      OLD STUFF WHICH MIGHT TURN OUT TO BE USEFUL
   ***********************************************************/
  /*
  // === Test custom boundary condition ===
  @Ignore
  @Test def PAD_1D_CUSTOM_Pos_1(): Unit = {
    val Y = i
    val gold = Array(Y,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,Y)
    //               ^                                 ^

    // The custom function replaces the elements outside the domain by the middle of the domain
    validate1D(gold, 1, (idx: ArithExpr, len: ArithExpr) =>
      (idx lt Cst(0)) ?? (len / 2) !! ((idx ge len) ?? (len / 2) !! idx))
  }

  @Ignore
  @Test def PAD_1D_CUSTOM_Pos_2(): Unit = {
    val Y = i
    val gold = Array(Y,Y,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,Y,Y)
    //               ^ ^                                 ^ ^

    // The custom function replaces the elements outside the domain by the middle of the domain
    validate1D(gold, 2, (idx: ArithExpr, len: ArithExpr) =>
      (idx lt Cst(0)) ?? (len / 2) !! ((idx ge len) ?? (len / 2) !! idx))
  }

  @Ignore
  @Test def PAD_2D_CUSTOM_Pos_1(): Unit = {
    val Y = i
    val gold = Array(
      Y, Y, Y, Y, Y, Y, // <
      Y, a, b, c, d, Y,
      Y, e, f, g, h, Y,
      Y, i, j, k, l, Y,
      Y, m, n, o, p, Y,
      Y, Y, Y, Y, Y, Y) // <
    //^              ^

    // The custom function replaces the elements outside the domain by the middle of the domain
    validate2D(gold, 1, (idx: ArithExpr, len: ArithExpr) =>
      (idx lt Cst(0)) ?? (len / 2) !! ((idx ge len) ?? (len / 2) !! idx))
  }

  @Ignore
  @Test def PAD_2D_CUSTOM_Pos_2(): Unit = {
    val Y = 123f
    val gold = Array(
      Y, Y, Y, Y, Y, Y, Y, Y, // <
      Y, Y, Y, Y, Y, Y, Y, Y, // <
      Y, Y, a, b, c, d, Y, Y,
      Y, Y, e, f, g, h, Y, Y,
      Y, Y, i, j, k, l, Y, Y,
      Y, Y, m, n, o, p, Y, Y,
      Y, Y, Y, Y, Y, Y, Y, Y, // <
      Y, Y, Y, Y, Y, Y, Y, Y) // <
    //^  ^              ^  ^

    throw new RuntimeException("Test case not implemented")
  }
    */
  // *** STAGE 2: corner cases ***

  // Pad using padding elements when padding > n
  // testing with n = 4, padding = 8
  /*@Ignore
  @Test def PAD_PadPadding_CONSTANT(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(X,X,X,X,X,X,X,X,a,b,c,d,X,X,X,X,X,X,X,X)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    validate1D(gold, 8, CONSTANT(X), input)
  }*/

  /*
  // *** STAGE 3: invalid usages ***
  @Ignore
  @Test def PAD_PadPadding_BOUNCE(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(D,B,A,B,C,D,C,B,a,b,c,d,C,B,A,B,C,D,C,B)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    validate1D(gold, 8, Bounce, input)
  }

  // Padding an empty array
  @Ignore
  @Test def PAD_EmptyArray(): Unit = {
    throw new RuntimeException("Test case not implemented")
  }
    // *** STAGE 1: common usage/basic functionalities ***

  // === No effect ===
  @Ignore
  @Test def PAD_NoEffect(): Unit = {
    val gold = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

    throw new RuntimeException("Test case not implemented")
  }

  // === Test constant boundary condition ===
  @Ignore
  @Test def PAD_1D_CONSTANT_Pos_1(): Unit = {
    val gold = Array(X,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,X)
    //               ^                                 ^

    validate1D(gold, 1, CONSTANT(X))
  }

  @Ignore
  @Test def PAD_1D_CONSTANT_Pos_2(): Unit = {
    val gold = Array(X,X,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,X,X)
    //               ^ ^                                 ^ ^

    validate1D(gold, 2, CONSTANT(X))
  }

  @Ignore
  @Test def PAD_2D_CONSTANT_Pos_1(): Unit = {
    val gold = Array(
      X, X, X, X, X, X, // <
      X, a, b, c, d, X,
      X, e, f, g, h, X,
      X, i, j, k, l, X,
      X, m, n, o, p, X,
      X, X, X, X, X, X) // <
    //^              ^

    validate2D(gold, 1, CONSTANT(X))
  }

  @Ignore
  @Test def PAD_2D_CONSTANT_Pos_2(): Unit = {
    val gold = Array(
      X, X, X, X, X, X, X, X, // <
      X, X, X, X, X, X, X, X, // <
      X, X, a, b, c, d, X, X,
      X, X, e, f, g, h, X, X,
      X, X, i, j, k, l, X, X,
      X, X, m, n, o, p, X, X,
      X, X, X, X, X, X, X, X, // <
      X, X, X, X, X, X, X, X) // <
    //^  ^              ^  ^

    validate2D(gold, 2, CONSTANT(X))
  // === Test bounce boundary condition ===
  @Ignore
  @Test def PAD_1D_BOUNCE_Pos_1(): Unit = {
    val gold = Array(B,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,O)
    //               ^                                 ^

    validate1D(gold, 1, Bounce)
  }

  @Ignore
  @Test def PAD_1D_BOUNCE_Pos_2(): Unit = {
    val gold = Array(C,B,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,O,N)
    //               ^ ^                                 ^ ^

    validate1D(gold, 2, Bounce)
  }

  @Ignore
  @Test def PAD_2D_BOUNCE_Pos_1(): Unit = {
    val gold = Array(
      F, E, F, G, H, G, // <
      B, a, b, c, d, C,
      F, e, f, g, h, G,
      J, i, j, k, l, K,
      N, m, n, o, p, O,
      J, I, J, K, L, K) // <
    //^              ^

    validate2D(gold, 1, Bounce)
  }

  @Ignore
  @Test def PAD_2D_BOUNCE_Pos_2(): Unit = {
    val gold = Array(
      K, J, I, J, K, L, K, J, // <
      G, F, E, F, G, H, G, F, // <
      C, B, a, b, c, d, C, B,
      G, F, e, f, g, h, G, F,
      K, J, i, j, k, l, K, J,
      O, N, m, n, o, p, O, N,
      K, J, I, J, K, L, K, J, // <
      G, F, E, F, G, H, G, F) // <
    //^  ^              ^  ^

    validate2D(gold, 2, Bounce)
  }
  }*/
}
