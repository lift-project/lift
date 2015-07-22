package exploration

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.{Test, AfterClass, BeforeClass}
import org.junit.Assert._
import opencl.ir.pattern._

abstract class RewriteRule {
  def apply(expr: Lambda): Lambda = {
    Lambda(expr.params, actOn(expr.body))
  }

  protected def actOn(expr: Expr): Expr
}

// rewrites:
// fun(x => Map(f)(x)) into fun(x => MapGlb(f)(x))
object MapToMapGlb extends RewriteRule {

  val pattern = "Map"
  val actOn = (m: Map) => MapGlb(m.f)

  override protected def actOn(expr: Expr): Expr =
    expr match {
      case FunCall(Map(l), args) =>
        MapGlb(l)(args)
      case _ => expr
    }
}

// rewrites:
// fun(x => Map(f)(x)) into fun(x => (Join() o Map(Map(f)) o Split(I))(x))
object MapToSplitMapMapJoin extends RewriteRule {
  override protected def actOn(expr: Expr): Expr =
    expr match {
      case FunCall(Map(l), args) =>
        (Join() o MapGlb(MapSeq(l)) o Split(4))(args)
      case _ => expr
    }
}

object TestRewrite {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }

  private def rewrite(expr: Lambda): List[Lambda] = {
    var lambdaList = List[Lambda]()

    // Map(f) => MapGlb(f)
    expr match {
      case Lambda(params, FunCall(Map(l), args)) =>
        lambdaList = Lambda(params, MapGlb(l)(args)) :: lambdaList
      case _ =>
    }

    // Map(f) => Join() o Map(Map(f)) o Split(I)
    expr match {
      case Lambda(params, FunCall(Map(l), args)) =>
        lambdaList = Lambda(params, (Join() o MapGlb(MapSeq(l)) o Split(4))(args)) :: lambdaList
      case _ =>
    }

    // Reduce(f) => toGlobal(MapSeq(id)) ReduceSeq(f)
    expr match {
      case Lambda(params, FunCall(Lambda(innerParams, FunCall(Reduce(l), innerArgs @ _*)) , arg)) =>
        lambdaList = Lambda(params, FunCall(toGlobal(MapSeq(id)) o Lambda(innerParams, ReduceSeq(l)(innerArgs:_*)), arg)) :: lambdaList
      case _ =>
    }

    // Map(f) => asScalar() o MapGlb(f.vectorize(4)) o asVector(4)
    expr match {
      case Lambda(params, FunCall(Map(Lambda(innerParams, FunCall(uf: UserFun, innerArg))), arg)) =>
        val vectorWidth = 4
        lambdaList = Lambda(params, (asScalar() o MapGlb(uf.vectorize(vectorWidth)) o asVector(vectorWidth))(arg)) :: lambdaList
      case _ =>
    }

    expr match {
      case Lambda(lambdaParams, FunCall(CompFun(params, functions @ _*), arg)) =>

        functions.sliding(2).foreach {

          // Join() o Split(_) => id
          case list @ List(Lambda(_, FunCall(Join(), _)), Lambda(_, FunCall(Split(_), _))) =>
            val newCfLambda: Lambda = applyCompFunRule(lambdaParams, params, functions, arg, list)
            lambdaList = newCfLambda :: lambdaList

          // Split(n) o Join(n) => id
          case list @ List(Lambda(_, FunCall(Split(splitChunkSize), _)), Lambda(_, FunCall(Join(), joinArg))) =>
            joinArg.t match {
              case ArrayType(ArrayType(_, joinChunkSize), _) =>
                if (joinChunkSize == splitChunkSize) {
                  val newCfLambda: Lambda = applyCompFunRule(lambdaParams, params, functions, arg, list)
                  lambdaList = newCfLambda :: lambdaList
                }
              case _ =>
            }

          // joinVec o splitVec => id
          case list @ List(Lambda(_, FunCall(asScalar(), _)), Lambda(_, FunCall(asVector(_), _))) =>
            val newCfLambda: Lambda = applyCompFunRule(lambdaParams, params, functions, arg, list)
            lambdaList = newCfLambda :: lambdaList

          // splitVec(n) o joinVec(n) => id
          case list @ List(Lambda(_, FunCall(asVector(splitVectorWidth), _)), Lambda(_, FunCall(asScalar(), joinArg))) =>
            joinArg.t match {
              case ArrayType(VectorType(_, joinVectorWidth), _) =>
                if (joinVectorWidth == splitVectorWidth) {
                  val newCfLambda: Lambda = applyCompFunRule(lambdaParams, params, functions, arg, list)
                  lambdaList = newCfLambda :: lambdaList
                }
              case _ =>
            }

          // ReduceSeq o MapSeq fusion
          case list @ List(Lambda(reduceParams, FunCall(ReduceSeq(Lambda(accNew, FunCall(redFun, redFunArgs @ _*))), reduceArgs @ _*)),
          Lambda(_, FunCall(MapSeq(mapLambda), _))) =>

            val newReduceFunArgs = redFunArgs.map(Expr.replace(_, accNew(1), mapLambda(accNew(1))))
            val replacement = Seq(Lambda(reduceParams, ReduceSeq(Lambda(accNew, redFun(newReduceFunArgs: _*)))(reduceArgs:_*)))
            val newCfLambda: Lambda = applyCompFunRule(lambdaParams, params, functions, arg, list, replacement)

            lambdaList = newCfLambda :: lambdaList
          case _ =>

        }
      case _ =>
    }

    lambdaList
  }

  def applyCompFunRule(lambdaParams: Array[Param], params: Array[Param], funs: Seq[Lambda],
                       arg: Expr, list: Seq[Lambda] with List[Any], seq: Seq[Lambda] = Seq()): Lambda = {
    val newList = funs.patch(funs.indexOfSlice(list), seq, 2)
    // TODO: get rid of cf and extra lambda if just one left
    Lambda(lambdaParams, CompFun(params, newList: _*).apply(arg))
  }
}

class TestRewrite {
  val N = Var("N")
  val A = Array.fill[Float](128)(0.5f)

  @Test
  def simpleMapTest(): Unit = {

    def f = fun(
      ArrayType(Float, N),
      input => Map(id) $ input
    )

    def goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val lambdaOptions = TestRewrite.rewrite(f)
    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })

    val l = MapToMapGlb(f)
    val (result: Array[Float], _) = Execute(128)(l, A)
    assertArrayEquals(l + " failed", gold, result, 0.0f)

  }

  @Test
  def slightlyMoreComplexMap(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      Float,
      (input, a) => MapGlb(fun(x => add(x, a))) $ input
    )

    def f = fun(
      ArrayType(Float, N),
      Float,
      (input, a) => Map(fun(x => add(a, x))) $ input
    )

    val a = 1.0f
    val (gold: Array[Float], _) = Execute(128)(goldF, A, a)
    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def joinSplit(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(Float, N),
      input => MapGlb(id) o Join() o Split(8) $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)
    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def splitJoin(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => MapGlb(MapSeq(id)) o Split(4) o Join() $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def asScalarAsVector(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(Float, N),
      input => MapGlb(id) o asScalar() o asVector(8) $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)
    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def asVectorAsScalar(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(VectorType(Float, 4), N),
      input => MapGlb(id.vectorize(4)) o asVector(4) o asScalar() $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A.flatten)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def simpleReduceTest(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ input
    )

    val f = fun(
      ArrayType(Float, N),
      input => Reduce(add, 0.0f) $ input
    )

    val lambdaOptions = TestRewrite.rewrite(f)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def ReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, plusOne(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayType(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(plusOne) $ input
    )

    val lambdaOptions = TestRewrite.rewrite(f)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }


  @Test
  def ReduceSeqMapSeqChangesType(): Unit = {

    val userFun = UserFun("idIntToFloat", "x", "{ return x; }", Int, Float)

    val goldF = fun(
      ArrayType(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, userFun(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayType(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(userFun) $ input
    )

    val A = Array.tabulate(128)(i => i)

    val lambdaOptions = TestRewrite.rewrite(f)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def ReduceSeqMapSeqArray(): Unit = {

    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) o fun(elem => Zip(acc, MapSeq(plusOne) $ elem)) $ elem),
          Value(0.0f, ArrayType(Float, 4))) $ input
    )

    val f = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) $ Zip(acc, elem)),
          Value(0.0f, ArrayType(Float, 4))) o MapSeq(MapSeq(plusOne)) $ input
    )

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)

//    lambdaOptions.foreach(l => {
//      val (result: Array[Float], _) = Execute(1, 1)(l, A)
//      assertArrayEquals(l + " failed", gold, result, 0.0f)
//    })
  }

  @Test
  def moreComplexReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, add(newValue, a))), 0.0f) $ input
    )

    val f = fun(
      ArrayType(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(fun(x => add(x, a))) $ input
    )

    val a = 2.0f

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A, a)

    val lambdaOptions = TestRewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }


}
