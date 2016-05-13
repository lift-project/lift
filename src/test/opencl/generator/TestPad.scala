package opencl.generator

import apart.arithmetic.{Cst, ArithExpr, Var}
import ir.ArrayType
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._


object TestPad {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestPad {
  import Pad.Boundary._

  // use letters instead of numbers
  val next = { var n = -1f; () => { n = n + 1f; n } }
  val a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p = next()
  // same thing with uppercase to distinguish padding
  val Next = { var n = -1f; () => { n = n + 1f; n } }
  val A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P  = Next()

  /// Input array
  val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
  val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}

  def validate1D(gold: Array[Float], left: Int, right: Int, boundary: Pad.BoundaryFun, input: Array[Float] = input) = {
    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(id) o Pad(left, right, boundary) $ domain
    )

    val (output: Array[Float],runtime) = Execute(input.length, input.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  def validate2D(gold: Array[Float],
                 top: Int, bottom: Int,
                 left: Int, right: Int,
                 boundary: Pad.BoundaryFun) = {
    val N = Var("N")
    val fct = fun(
      ArrayType(ArrayType(Float, N), N),
      (domain) => MapGlb(0)(
        MapGlb(1)(id)) o
         Pad2D(top, bottom, left, right, boundary) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length,gold.length)(fct, input2D)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  def validate2DOriginal(gold: Array[Float], left: Int, right: Int, boundary: Pad.BoundaryFun) = {
    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(id) o Join()
        o Transpose() o Pad(left, right, boundary)
        o Transpose() o Pad(left, right, boundary)
        o Split(4) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length,gold.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  /* **********************************************************
        PAD 1D
     ***********************************************************/
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
        OLD STUFF WHICH MIGHT TURN OUT USEFUL
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