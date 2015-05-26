package opencl.generator

import arithmetic.Var
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

// --8<----8<----8<----8<----8<----8<--
// TODO(tlutz) delete this for master
// feature implementation switches
object Feature {
  def requires(flag: Boolean) = {
    Assume.assumeTrue("test disabled: Feature not yet implemented", flag)
  }

  // Array size
  val ARRAY_1D = false
  val ARRAY_2D = false

  // Boundary conditions
  val BOUNDARY_CONSTANT = false
  val BOUNDARY_CLAMP = false
  val BOUNDARY_BOUNCE = false
  val BOUNDARY_MIRROR = false
  val BOUNDARY_CUSTOM = false
  val BOUNDARY_WRAP = false

  // Corner cases
  val OVERSIZED_PAD = false
}

// TODO(tlutz) missing tests
// - negative padding

// --8<----8<----8<----8<----8<----8<--

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

  // *** STAGE 1: common usage/basic functionalities ***

  // === No effect ===
  @Ignore
  @Test def PAD_NoEffect(): Unit = {
    val gold = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

    throw new RuntimeException("Test case not implemented")
  }

  // === Test constant boundary condition ===
  @Test def PAD_1D_CONSTANT_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_CONSTANT & Feature.ARRAY_1D)

    val gold = Array(X,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,X)
    //               ^                                 ^

    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(id) o Pad(1, CONSTANT(X)) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def PAD_1D_CONSTANT_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_CONSTANT & Feature.ARRAY_1D)

    val gold = Array(X,X,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,X,X)
    //               ^ ^                                 ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_CONSTANT_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_CONSTANT & Feature.ARRAY_2D)

    val gold = Array(
      X, X, X, X, X, X, // <
      X, a, b, c, d, X,
      X, e, f, g, h, X,
      X, i, j, k, l, X,
      X, m, n, o, p, X,
      X, X, X, X, X, X) // <
    //^              ^

    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => Join() o MapGlb(id)
        o Transpose() o Pad(1, CONSTANT(X))
        o Transpose() o Pad(1, CONSTANT(X))
        o Split(4) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def PAD_2D_CONSTANT_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_CONSTANT & Feature.ARRAY_2D)
    
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

    throw new RuntimeException("Test case not implemented")
  }

  // === Test clamp boundary condition ===
  @Test def PAD_1D_CLAMP_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_CLAMP & Feature.ARRAY_1D)
    
    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    //               ^                                 ^

    val fct = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(id) o Pad(1, CLAMP) $ domain
    )

    val (output: Array[Float],runtime) = Execute(gold.length)(fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def PAD_1D_CLAMP_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_CLAMP & Feature.ARRAY_1D)

    val gold = Array(A,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,P)
    //               ^ ^                                 ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_CLAMP_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_CLAMP & Feature.ARRAY_2D)

    val gold = Array(
      A, A, B, C, D, D, // <
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P) // <
    //^              ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_CLAMP_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_CLAMP & Feature.ARRAY_2D)

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

    throw new RuntimeException("Test case not implemented")
  }

  // === Test bounce boundary condition ===
  @Test def PAD_1D_BOUNCE_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_BOUNCE & Feature.ARRAY_1D)

    val gold = Array(B,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,O)
    //               ^                                 ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_1D_BOUNCE_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_BOUNCE & Feature.ARRAY_1D)

    val gold = Array(C,B,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,O,N)
    //               ^ ^                                 ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_BOUNCE_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_BOUNCE & Feature.ARRAY_2D)

    val gold = Array(
      F, E, F, G, H, G, // <
      B, a, b, c, d, C,
      F, e, f, g, h, G,
      J, i, j, k, l, K,
      N, m, n, o, p, O,
      J, I, J, K, L, K) // <
    //^              ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_BOUNCE_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_BOUNCE & Feature.ARRAY_2D)

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

    throw new RuntimeException("Test case not implemented")
  }

  // === Test mirror boundary condition ===
  @Test def PAD_1D_MIRROR_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_MIRROR & Feature.ARRAY_1D)

    val gold = Array(A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P)
    //               ^                                 ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_1D_MIRROR_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_MIRROR & Feature.ARRAY_1D)

    val gold = Array(B,A,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,P,O)
    //               ^ ^                                 ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_MIRROR_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_MIRROR & Feature.ARRAY_2D)

    val gold = Array(
      A, A, B, C, D, D, // <
      A, a, b, c, d, D,
      E, e, f, g, h, H,
      I, i, j, k, l, L,
      M, m, n, o, p, P,
      M, M, N, O, P, P) // <
    //^              ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_MIRROR_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_MIRROR & Feature.ARRAY_2D)

    val gold = Array(
      F, E, E, F, G, H, H, G, // <
      B, A, A, B, C, D, D, C, // <
      B, A, a, b, c, d, D, C,
      F, E, e, f, g, h, H, G,
      J, I, i, j, k, l, L, K,
      N, M, m, n, o, p, P, O,
      N, M, M, N, O, P, P, O, // <
      M, I, I, J, K, L, L, K) // <
    //^  ^              ^  ^

    throw new RuntimeException("Test case not implemented")
  }

  // === Test wrap boundary condition ===
  @Test def PAD_1D_WRAP_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_WRAP & Feature.ARRAY_1D)

    val gold = Array(P,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,A)
    //               ^                                 ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_1D_WRAP_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_WRAP & Feature.ARRAY_1D)

    val gold = Array(O,P,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,A,B)
    //               ^ ^                                 ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_WRAP_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_WRAP & Feature.ARRAY_2D)

    val gold = Array(
      P, M, N, O, P, M, // <
      D, a, b, c, d, A,
      H, e, f, g, h, E,
      L, i, j, k, l, I,
      P, m, n, o, p, M,
      D, A, B, C, D, A) // <
    //^              ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_WRAP_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_WRAP & Feature.ARRAY_2D)

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

    throw new RuntimeException("Test case not implemented")
  }

  // === Test custom boundary condition ===
  @Test def PAD_1D_CUSTOM_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_CUSTOM & Feature.ARRAY_1D)

    val Y = 123f
    val gold = Array(Y,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,Y)
    //               ^                                 ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_1D_CUSTOM_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_CUSTOM & Feature.ARRAY_1D)

    val Y = 123f
    val gold = Array(Y,Y,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,Y,Y)
    //               ^ ^                                 ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_CUSTOM_Pos_1(): Unit = {
    Feature.requires(Feature.BOUNDARY_CUSTOM & Feature.ARRAY_2D)

    val Y = 123f
    val gold = Array(
      Y, Y, Y, Y, Y, Y, // <
      Y, a, b, c, d, Y,
      Y, e, f, g, h, Y,
      Y, i, j, k, l, Y,
      Y, m, n, o, p, Y,
      Y, Y, Y, Y, Y, Y) // <
    //^              ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_2D_CUSTOM_Pos_2(): Unit = {
    Feature.requires(Feature.BOUNDARY_CUSTOM & Feature.ARRAY_2D)

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
  @Test def PAD_PadPadding_CONSTANT(): Unit = {
    Feature.requires(Feature.OVERSIZED_PAD & Feature.BOUNDARY_CONSTANT & Feature.ARRAY_1D)

    val input = Array(a,b,c,d)
    val gold  = Array(X,X,X,X,X,X,X,X,a,b,c,d,X,X,X,X,X,X,X,X)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_PadPadding_CLAMP(): Unit = {
    Feature.requires(Feature.OVERSIZED_PAD & Feature.BOUNDARY_CLAMP & Feature.ARRAY_1D)

    val input = Array(a,b,c,d)
    val gold  = Array(A,A,A,A,A,A,A,A,a,b,c,d,D,D,D,D,D,D,D,D)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_PadPadding_BOUNCE(): Unit = {
    Feature.requires(Feature.OVERSIZED_PAD & Feature.BOUNDARY_BOUNCE & Feature.ARRAY_1D)

    val input = Array(a,b,c,d)
    val gold  = Array(D,B,A,B,C,D,C,B,a,b,c,d,C,B,A,B,C,D,C,B)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_PadPadding_MIRROR(): Unit = {
    Feature.requires(Feature.OVERSIZED_PAD & Feature.BOUNDARY_MIRROR & Feature.ARRAY_1D)

    val input = Array(a,b,c,d)
    val gold  = Array(A,B,C,D,D,C,B,A,a,b,c,d,D,C,B,A,A,B,C,D)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  @Test def PAD_PadPadding_WRAP(): Unit = {
    Feature.requires(Feature.OVERSIZED_PAD & Feature.BOUNDARY_WRAP & Feature.ARRAY_1D)

    val input = Array(a,b,c,d)
    val gold  = Array(A,B,C,D,A,B,C,D,a,b,c,d,A,B,C,D,A,B,C,D)
    //                ^ ^ ^ ^ ^ ^ ^ ^         ^ ^ ^ ^ ^ ^ ^ ^

    throw new RuntimeException("Test case not implemented")
  }

  // *** STAGE 3: invalid usages ***

  // Padding an empty array
  @Ignore
  @Test def PAD_EmptyArray(): Unit = {
    throw new RuntimeException("Test case not implemented")
  }
}