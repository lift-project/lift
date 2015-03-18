package ir

import org.junit.Assert._
import org.junit.Test

import scala.util.Random



class TestExpr {

  private val rnd = new Random(0)

  private def rndPositive() : Int = {
    val r = rnd.nextInt(5)
    if (r == 0)
      r+1
    else if (r < 0)
      -r
    else
      r
  }

  private def rndExpr(maxDepth: Int, depth: Int=0) : ArithExpr = {

    if (depth > maxDepth)
      return Cst(rndPositive())

    Random.nextInt(3) match {
      case 0 => rndExpr(maxDepth, depth+1) * rndExpr(maxDepth, depth+1)
      case 1 => rndExpr(maxDepth, depth+1) + rndExpr(maxDepth, depth+1)
      case 2 => rndExpr(maxDepth, depth+1) / rndExpr(maxDepth, depth+1)
    }
  }

  @Test def testRandom() {

    for (a <- 1 to 10) {
      val re = rndExpr(3)
      println("random expression = "+re)

      val oriEval = re.evalDbl()
      val sim = ExprSimplifier.simplify(re)
      println("simplified expression = "+sim)

      val simEval = sim.evalDbl()
      assert(math.abs(oriEval-simEval) <= 1.0/1000000.0, oriEval+" != "+simEval)
    }


  }

  @Test def testSimplifcation() {

    testRandom()

    val c0 = Cst(0)
    val c1 = Cst(1)
    val c2 = Cst(2)
    val c10 = Cst(10)
    //val v = Var(new RangeMul(c0,c2,c10))
    val e = (c0+c1)*(c10+c2)+(c10/c2)

    ExprSimplifier.simplify(e).eval()

    assertEquals(e.eval(), ExprSimplifier.simplify(e).eval())

    //println(e.eval())
    val result = ExprSimplifier.simplify(e)
    //println(result.eval())

    assertEquals(Cst(17),result)

    //val e2 = (c0/v+c1/v)*v*(c10+c2)+(c10/c2)
    //val result2 = ExprSimplifier.simplify(e)
    //println(result2)
  }

  @Test def Division(): Unit = {
    val i = Var("i")
    val M = Var("M")
    assertEquals(i + i % M, ExprSimplifier.simplify(i / M * M + i % M))
  }

  @Test def Fraction(): Unit = {
    val i = Var("i")
    val M = Var("M")
    assertEquals((i div M) * M + i % M, ExprSimplifier.simplify((i div M) * M + i % M))
  }

  @Test def modOfVarWithVarRange(): Unit = {
    val M = Var("M")
    val i = Var(GoesToRange(M))
    assertEquals(i, ExprSimplifier.simplify(i % M))
  }

  @Test def modOfVarWithConstantRange(): Unit = {
    val c =  Cst(10)
    val i = Var(GoesToRange(c))
    assertEquals(i, ExprSimplifier.simplify(i % c))
  }

  @Test def modOfVarWithConstantRange2(): Unit = {
    val i = Var(GoesToRange(Cst(10)))
    assertEquals(i, ExprSimplifier.simplify(i % Cst(128)))
  }

  @Test def modSameDividendDivisor(): Unit = {
    val N = Var("N")
    assertEquals(Cst(0), ExprSimplifier.simplify(N % N))
  }

  @Test def modDivisorZero(): Unit = {
    val N = Var("N")
    assertEquals(Cst(0), ExprSimplifier.simplify(0 % N))
  }

  @Test def modConstants(): Unit = {
    assertEquals(Cst(1), ExprSimplifier.simplify(Cst(11) % Cst(2)))
  }

  @Test def prodMultipleOfVar(): Unit = {
    val N = Var("N")
    val M = Var("M")
    assertTrue(ArithExpr.multipleOf(N*M, N))
  }

  @Test def prodMultipleOfProd(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")
    assertTrue(ArithExpr.multipleOf(N*M*K, N*K))
  }

  @Test def divConstants(): Unit = {
    assertEquals(Cst(2), ExprSimplifier.simplify(Cst(4) div Cst(2)))
  }

  @Test def prodDivConstant(): Unit = {
    val N = Var("N")
    assertEquals(Cst(2) * N, ExprSimplifier.simplify(Cst(4) * N div Cst(2)))
  }

  @Test def prodDivProdWithConstants(): Unit = {
    val N = Var("N")
    val M = Var("M")
    assertEquals(Cst(2) * N div M, ExprSimplifier.simplify(Cst(4) * N div (Cst(2) * M)))
  }

  @Test def prodDivProdWithConstants2(): Unit = {
    val N = Var("N")
    val M = Var("M")
    assertEquals(N div (M * Cst(2)), ExprSimplifier.simplify(Cst(2) * N div (Cst(4) * M)))
  }

  @Test def prodDivProdWithConstants3(): Unit = {
    val N = Var("N")
    val M = Var("M")
    assertEquals((N * Cst(2)) div (M * Cst(3)), ExprSimplifier.simplify(Cst(2) * N div (Cst(3) * M)))
  }

  @Test def prodNotMultipleOfProd(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")
    assertFalse(ArithExpr.multipleOf(N*M, N*K))
  }

  @Test def cstMultipleOfCst(): Unit = {
    assertTrue(ArithExpr.multipleOf(Cst(8), Cst(4)))
  }

  @Test def prodMultipleOfCst(): Unit = {
    assertTrue(ArithExpr.multipleOf(Cst(8)*Var("i"), Cst(4)))
  }

  @Test def varWithConstantRangeDivConstant(): Unit = {
    val i = Var(RangeAdd(Cst(0), Cst(4), Cst(1)))
    assertEquals(Cst(0), ExprSimplifier.simplify(i div Cst(4)))
  }

  @Test def sumVarsDivConstant(): Unit = {
    val i = Var(GoesToRange(Cst(4)))
    val j = Var(GoesToRange(Var("M")))
    assertEquals(Cst(4) * j, ExprSimplifier.simplify((i + Cst(16) * j ) div Cst(4)))
  }

  @Test def simplifyProdEmpty(): Unit = {
    assertEquals(Cst(1), ExprSimplifier.simplify(Prod(List[ArithExpr]())))
  }

  @Test def modSum(): Unit = {
    val N = Var("N")
    assertEquals(1 % N, ExprSimplifier.simplify((N + 1) % N))
  }

  @Test def modProd(): Unit = {
    val N = Var("N")
    val M = Var("M")
    assertEquals(Cst(0), ExprSimplifier.simplify((N * M) % N))
  }

  @Test def OneByOne() {
    assertEquals(Cst(1), ExprSimplifier.simplify(Cst(1) / Cst(1)))
  }

  @Test def NByN() {
    val N = Var("N")
    assertEquals(Cst(1), ExprSimplifier.simplify(N / N))
  }

  @Test def zeroTimesVar(): Unit = {
    val N = Var("N")
    assertEquals(Cst(0), ExprSimplifier.simplify(Cst(0) * N))
  }

  @Test def zeroTimesTwo(): Unit = {
    assertEquals(Cst(0), ExprSimplifier.simplify(Cst(0) * Cst(2)))
  }

  @Test def simplifySumTwoMinusTwo(): Unit ={
    assertEquals(Cst(0), ExprSimplifier.simplify(Cst(2) - Cst(2)))
  }

  @Test def simplifySumNMinusN(): Unit ={
    val N = Var("N")
    assertEquals(Cst(0), ExprSimplifier.simplify(N - N))
  }

  @Test def simplifySumZeroProducts(): Unit ={
    val N = Var("N")
    assertEquals(Cst(0), ExprSimplifier.simplify(4*N - 4*N))
  }

  @Test def simplifySumTwoPlusFive(): Unit ={
    assertEquals(Cst(7), ExprSimplifier.simplify(Cst(2) + Cst(5)))
  }

  @Test def prodEqualsConstants(): Unit = {
    assertEquals(Cst(2) * Cst(1), Cst(2) * Cst(1))
    assertEquals(Cst(1) * Cst(2), Cst(2) * Cst(1))
  }

  @Test def prodEqualsVars(): Unit = {
    val N = Var("N")
    assertEquals(Cst(2) * N, Cst(2) * N)
    assertEquals(N * Cst(2), Cst(2) * N)
  }

  @Test def sumEqualsConstants(): Unit = {
    assertEquals(Cst(2) + Cst(1), Cst(2) + Cst(1))
    assertEquals(Cst(1) + Cst(2), Cst(2) + Cst(1))
  }

  @Test def sumEqualsVars(): Unit = {
    val N = Var("N")
    assertEquals(Cst(2) + N, Cst(2) + N)
    assertEquals(N + Cst(2), Cst(2) + N)
  }

  @Test def powSimplify(): Unit = {
    val N = Var("N")
    val expr = Pow( 1*1*Pow(2, -1), Log(2, N) + (1  * -1) ) * N
    assertEquals(Cst(2), ExprSimplifier.simplify(expr))
  }

  @Test def minusOneTimesMinusFive(): Unit = {
    assertEquals(Cst(5), ExprSimplifier.simplify(Cst(-1) * Cst(-5)))
  }

  @Test def simplifyAccess(): Unit = {
    val M = Var(StartFromRange(Cst(1)))
    val N = Var(StartFromRange(Cst(1)))

    val wg_id_0 = Var("wid_0",ContinousRange(0, N / 2))
    val wg_id_1 = Var("wid_1",ContinousRange(0, M / 4))
    val l_id_0 = Var("lid_0",ContinousRange(0, 2))
    val l_id_1 = Var("lid_0",ContinousRange(0, 4))

    val firstRead = (
      (wg_id_0 * 1 * M / (4) * 2 * 4) +
        (((wg_id_1 * 1 * 2 * 4) +
          (l_id_0 * 1 * 4) + (l_id_1 * 1) +
          0) /
          ((4 * 1)) / (2) * 4 * 1) +
        (((((wg_id_1 * 1 * 2 * 4) +
          (l_id_0 * 1 * 4) +
          (l_id_1 * 1) + 0) /
          ((4 * 1))) %
          2) *
          M / (4) * 4 * 1) +
        (((wg_id_1 * 1 * 2 * 4) +
          (l_id_0 * 1 * 4) + (l_id_1 * 1) +
          0) %
          (4 * 1)))

    val simpFirstRead = ExprSimplifier.simplify(firstRead)
    println(firstRead)
    println(simpFirstRead)

    val firstWrite = ((((l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) /
      (1) / (M) * 1) +
      (((((l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) /
        (1)) %
        M) *
        N * 1) +
      (((l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) %
        1))

    val secondWrite = (
      (((wg_id_0 * 1 * M / (4) * 2 * 4) +
        (((wg_id_1 * 1 * 2 * 4) +
          (l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) /
          ((4 * 1)) / (2) * 4 * 1) +
        (((((wg_id_1 * 1 * 2 * 4) +
          (l_id_0 * 1 * 4) + (l_id_1 * 1) +
          0) /
          ((4 * 1))) %
          2) *
          M / (4) * 4 * 1) +
        (((wg_id_1 * 1 * 2 * 4) +
          (l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) %
          (4 * 1))) /
        (1) / (M) * 1) +
        (((((wg_id_0 * 1 * M / (4) * 2 * 4) +
          (((wg_id_1 * 1 * 2 * 4) +
            (l_id_0 * 1 * 4) + (l_id_1 * 1) +
            0) /
            ((4 * 1)) / (2) * 4 * 1) +
          (((((wg_id_1 * 1 * 2 * 4) +
            (l_id_0 * 1 * 4) + (l_id_1 * 1) +
            0) /
            ((4 * 1))) %
            2) *
            M / (4) * 4 * 1) +
          (((wg_id_1 * 1 * 2 * 4) +
            (l_id_0 * 1 * 4) + (l_id_1 * 1) +
            0) %
            (4 * 1))) /
          (1)) %
          M) *
          N * 1) +
        (((wg_id_0 * 1 * M / (4) * 2 * 4) +
          (((wg_id_1 * 1 * 2 * 4) +
            (l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) /
            ((4 * 1)) / (2) * 4 * 1) +
          (((((wg_id_1 * 1 * 2 * 4) +
            (l_id_0 * 1 * 4) + (l_id_1 * 1) +
            0) /
            ((4 * 1))) %
            2) *
            M / (4) * 4 * 1) +
          (((wg_id_1 * 1 * 2 * 4) +
            (l_id_0 * 1 * 4) + (l_id_1 * 1) + 0) %
            (4 * 1))) %
          1))
  }
}
