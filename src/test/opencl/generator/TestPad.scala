package opencl.generator

import apart.arithmetic.{Cst, ArithExpr, Var}
import ir.UserFunDef._
import ir.{Join, Split, ArrayType, fun}
import opencl.executor._
import opencl.ir._
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
  val next = { var n = 0f; () => { n = n + 1f; n } }
  val a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p = next()
  // same thing with uppercase to distinguish padding
  val Next = { var n = 0f; () => { n = n + 1f; n } }
  val A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P  = Next()

  /// Input array
  val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

  /// constant padding
  val X = 0f

  /// Cross validate 1D Pad
  /// @param gold Expected output
  /// @param size Padding offset
  /// @param boundary Boundary behavior
  def validate1D(gold: Array[Float], size: Int, boundary: Pad.BoundaryFct, input: Array[Float] = input) = {
    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(id) o Pad(size, boundary) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length, gold.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  /// Cross validate 2D pad
  /// @param gold Expected output
  /// @param size Padding offset
  /// @param boundary Boundary behavior
  def validate2D(gold: Array[Float], size: Int, boundary: Pad.BoundaryFct) = {
    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(id) o Join()
        o Transpose() o Pad(size, boundary)
        o Transpose() o Pad(size, boundary)
        o Split(4) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length,gold.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  // *** STAGE 1: common usage/basic functionalities ***

  // === No effect ===
  @Ignore
  @Test def PAD_NoEffect(): Unit = {
    val gold = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

    throw new RuntimeException("Test case not implemented")
  }

  // === Test constant boundary condition ===
  /*@Ignore
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
  }*/

  // === Test clamp boundary condition ===
  @Test def PAD_1D_CLAMP_Pos_1(): Unit = {
    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    //               ^                                 ^

    validate1D(gold, 1, Clamp)
  }

  @Test def PAD_1D_CLAMP_Pos_2(): Unit = {
    val gold = Array(A,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,P)
    //               ^ ^                                 ^ ^

    validate1D(gold, 2, Clamp)
  }

  @Test def PAD_2D_CLAMP_Pos_1(): Unit = {
    val gold = Array(
      A, A, B, C, D, D, // <
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P) // <
    //^              ^

    validate2D(gold, 1, Clamp)
  }

  @Test def PAD_2D_CLAMP_Pos_2(): Unit = {
    val gold = Array(
      A, A, A, B, C, D, D, D, // <
      A, A, A, B, C, D, D, D, // <
      A, A, a, b, c, d, D, D,
      E, E, e, f, g, h, H, H,
      I, I, i, j, k, l, L, L,
      M, M, m, n, o, p, P, P,
      M, M, M, N, O, P, P, P, // <
      M, M, M, N, O, P, P, P) // <
    //^  ^              ^  ^

    validate2D(gold, 2, Clamp)
  }

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

  // === Test mirror boundary condition ===
  @Test def PAD_1D_MIRROR_Pos_1(): Unit = {
    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    //               ^                                 ^

    validate1D(gold, 1, Mirror)
  }


  @Test def PAD_1D_MIRROR_Pos_2(): Unit = {
    val gold = Array(B,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,O)
    //               ^ ^                                 ^ ^

    validate1D(gold, 2, Mirror)
  }

  @Test def PAD_2D_MIRROR_Pos_1(): Unit = {
    val gold = Array(
      A, A, B, C, D, D, // <
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P) // <
    //^              ^

    validate2D(gold, 1, Mirror)
  }

  @Test def PAD_2D_MIRROR_Pos_2(): Unit = {
    val gold = Array(
      F, E, E, F, G, H, H, G, // <
      B, A, A, B, C, D, D, C, // <
      B, A, a, b, c, d, D, C,
      F, E, e, f, g, h, H, G,
      J, I, i, j, k, l, L, K,
      N, M, m, n, o, p, P, O,
      N, M, M, N, O, P, P, O, // <
      J, I, I, J, K, L, L, K) // <
    //^  ^              ^  ^

    validate2D(gold, 2, Mirror)
  }

  // === Test wrap boundary condition ===
  @Test def PAD_1D_WRAP_Pos_1(): Unit = {
    val gold = Array(P,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,A)
    //               ^                                 ^

    validate1D(gold, 1, Wrap)
  }


  @Test def PAD_1D_WRAP_Pos_2(): Unit = {
    val gold = Array(O,P,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,A,B)
    //               ^ ^                                 ^ ^

    validate1D(gold, 2, Wrap)
  }

  @Test def PAD_2D_WRAP_Pos_1(): Unit = {
    val gold = Array(
      P, M, N, O, P, M, // <
      D, a, b, c, d, A,
      H, e, f, g, h, E,
      L, i, j, k, l, I,
      P, m, n, o, p, M,
      D, A, B, C, D, A) // <
    //^              ^

    validate2D(gold, 1, Wrap)
  }

  @Test def PAD_2D_WRAP_Pos_2(): Unit = {
    val gold = Array(
      K, L, I, J, K, L, I, J, // <
      O, P, M, N, O, P, M, N, // <
      C, D, a, b, c, d, A, B,
      G, H, e, f, g, h, E, F,
      K, L, i, j, k, l, I, J,
      O, P, m, n, o, p, M, N,
      C, D, A, B, C, D, A, B, // <
      G, H, E, F, G, H, E, F) // <
    //^  ^              ^  ^

    validate2D(gold, 2, Wrap)
  }

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

  @Test def PAD_PadPadding_CLAMP(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(A,A,A,A,A,A,A,A,a,b,c,d,D,D,D,D,D,D,D,D)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    validate1D(gold, 8, Clamp, input)
  }

  @Ignore
  @Test def PAD_PadPadding_BOUNCE(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(D,B,A,B,C,D,C,B,a,b,c,d,C,B,A,B,C,D,C,B)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    validate1D(gold, 8, Bounce, input)
  }

  @Test def PAD_PadPadding_MIRROR(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(A,B,C,D,D,C,B,A,a,b,c,d,D,C,B,A,A,B,C,D)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    validate1D(gold, 8, Mirror, input)
  }

  @Test def PAD_PadPadding_WRAP(): Unit = {
    val input = Array(a,b,c,d)
    val gold  = Array(A,B,C,D,A,B,C,D,a,b,c,d,A,B,C,D,A,B,C,D)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    validate1D(gold, 8, Wrap, input)
  }

  // *** STAGE 3: invalid usages ***

  // Padding an empty array
  @Ignore
  @Test def PAD_EmptyArray(): Unit = {
    throw new RuntimeException("Test case not implemented")
  }
}