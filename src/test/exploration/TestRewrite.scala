package exploration

import arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.{Test, AfterClass, BeforeClass}
import org.junit.Assert._
import opencl.ir.pattern._

abstract class RewriteRule {
  def apply(expr: Lambda): Lambda
}

// rewrites:
// fun(x => Map(f)(x)) into fun(x => MapGlb(f)(x))
object MapToMapGlb extends RewriteRule {

  val pattern = "Map"
  val actOn = (m: Map) => MapGlb(m.f)

  override def apply(expr: Lambda): Lambda = {
    expr match {
      case Lambda(params, FunCall(Map(l), args)) =>
        Lambda(params, MapGlb(l)(args))
      case _ => expr
    }
  }
}

// rewrites:
// fun(x => Map(f)(x)) into fun(x => (Join() o Map(Map(f)) o Split(I))(x))
object MapToSplitMapMapJoin extends RewriteRule {
  override def apply(expr: Lambda): Lambda = {
    expr match {
      case Lambda(params, FunCall(Map(l), args)) =>
        Lambda(params, (Join() o MapGlb(MapSeq(l)) o Split(4))(args))
      case _ => expr
    }
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

    // Reduce(f) = >toGlobal(MapSeq(id)) ReduceSeq(f)
    expr match {
      case Lambda(params, FunCall(Lambda(innerParams, FunCall(Reduce(l), innerArgs @ _*)) , arg)) =>
        lambdaList = Lambda(params, FunCall(toGlobal(MapSeq(id)) o Lambda(innerParams, ReduceSeq(l)(innerArgs:_*)), arg)) :: lambdaList
      case _ =>
    }

    // Map(f) => asScalar() o MapGlb(f.vectorize(4)) o asVector(4)
    expr match {
      case Lambda(params, FunCall(Map(Lambda(innerParams, FunCall(uf: UserFun, innerArg))), arg)) =>
        lambdaList = Lambda(params, (asScalar() o MapGlb(uf.vectorize(4)) o asVector(4))(arg)) :: lambdaList
      case _ =>
    }

    // Join() o Split(_) => id
    expr match {
      case Lambda(lambdaParams, FunCall(CompFun(params, funs @ _*), arg)) =>
        funs.sliding(2).foreach {
          case list@List(Lambda(_, FunCall(Join(), _)), Lambda(_, FunCall(Split(_), _))) =>
            val newList = funs.patch(funs.indexOfSlice(list), Seq(), 2)
            // TODO: get rid of cf and extra lambda if just one left
            val newCfLambda = Lambda(lambdaParams, CompFun(params, newList: _*).apply(arg))
            lambdaList = newCfLambda :: lambdaList

          case _ =>
        }
      case _ =>
    }

    lambdaList
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

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def splitJoin(): Unit = {
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

}
