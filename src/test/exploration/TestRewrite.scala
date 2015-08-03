package exploration

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.{Test, AfterClass, BeforeClass}
import org.junit.Assert._
import opencl.ir.pattern._

object TestRewrite {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }

  case class Rule(desc: String,
                  rewrite: PartialFunction[Expr, Expr],
                  isValid: Context => Boolean = _ => true)

  val rules: Seq[Rule] =
    Seq(
      Rule("Iterate(0, _) => Epsilon", {
        case FunCall(Iterate(n, _), arg) if n.eval == 0 => arg
      }),

      Rule("Iterate(1, x) => x", {
        case FunCall(Iterate(n, f), arg) if n.eval == 1 => f(arg)
      }),

      Rule("Epsilon() o x => x", {
        case FunCall(Epsilon(), arg) => arg
      }),

      Rule("x o Epsilon() => x", {
        case FunCall(f, FunCall(Epsilon(), arg)) => f(arg)
      }),

      Rule("Map(Epsilon()) => Epsilon()", {
        case FunCall(Map(Lambda(_, FunCall(Epsilon(), _))), arg) => arg
      }),

      Rule("joinVec o splitVec => id", {
        case FunCall(asScalar(), FunCall(asVector(_), arg)) => arg
      }),

      Rule("splitVec(_) o joinVec() => id", {
        case FunCall(asVector(n), FunCall(asScalar(), arg))
          if (arg.t match {
            case ArrayType(VectorType(_, m), _) => n == m
            case _ => false
          }) => arg
      }),

      Rule("Join() o Split(_) => id", {
        case FunCall(Join(), FunCall(Split(_), arg)) => arg
      }),

      Rule("Split(_) o Join() => id", {
        case FunCall(Split(n), FunCall(Join(), arg))
          if (arg.t match {
            case ArrayType(ArrayType(_, m), _) => n == m
            case _ => false
          }) => arg
      }),

      Rule("Map(f) => MapSeq(f)", {
        case FunCall(Map(f), arg) => MapSeq(f)(arg)
      }),

      Rule("Map(f) => MapGlb(f)", {
        case FunCall(Map(f), arg)
          // check that none of these are nested inside
          if !f.body.contains({
              case FunCall(_:MapGlb, _) =>
              case FunCall(_:MapWrg, _) =>
              case FunCall(_:MapLcl, _) =>
              case FunCall(_:MapWarp, _) =>
              case FunCall(_:MapLane, _) =>
            })
          => MapGlb(f)(arg)

      }, c => !(c.inMapGlb || c.inMapWrg || c.inMapWarp)),

      Rule("Map(f) => MapWrg(f)", {
        case FunCall(Map(f), arg)
          // check that there is a nested map inside ...
          if f.body.contains({
                case FunCall(_:Map, _) =>
              }) &&
          // and that none of these are nested inside
             !f.body.contains({
                case FunCall(_:MapGlb, _) =>
                case FunCall(_:MapWrg, _) =>
              })
          => MapWrg(f)(arg)

      }, c => !(c.inMapGlb || c.inMapWrg || c.inMapWarp)),

      Rule("Map(f) => MapLcl(f)", {
        case FunCall(Map(f), arg)
          // check that none of these are nested inside
          if !f.body.contains({
              case FunCall(_:MapLcl, _) =>
              case FunCall(_:MapWarp, _) =>
              case FunCall(_:MapLane, _) =>
             })
          => MapLcl(f)(arg)
      }, c => c.inMapWrg && !c.inMapLcl),

      Rule("Map(f) => MapWarp(f)", {
        case FunCall(Map(f), arg)
          // check if there is a nested map inside
          if f.body.contains({
              case FunCall(_:Map, _) =>
            }) &&
          // and that none of these are nested inside
            !f.body.contains({
              case FunCall(_:MapGlb, _) =>
              case FunCall(_:MapWrg, _) =>
              case FunCall(_:MapLcl, _) =>
              case FunCall(_:MapWarp, _) =>
              case FunCall(_:MapLane, _) =>
            })
          => MapWarp(f)(arg)
      }, c => !(c.inMapGlb || c.inMapWrg || c.inMapWarp)),

      Rule("Map(f) => MapLane(f)", {
        case FunCall(Map(f), arg)
          // check that none of these are nested inside
          if !f.body.contains({
              case FunCall(_:MapLane) =>
            })
          => MapLane(f)(arg)
      }, c => c.inMapWarp && !c.inMapLane),

      Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
        case FunCall(Map(f), arg) =>
          Join() o Map(Map(f)) o Split(4) $ arg
      }),

      Rule("Reduce(f) => toGlobal(MapSeq(id)) ReduceSeq(f)", {
        case FunCall(Reduce(f), init, arg) =>
          toGlobal(MapSeq(id)) o ReduceSeq(f, init) $ arg
      }),

      Rule("Map(uf) => asScalar() o MapGlb(Vectorize(4)(uf)) o asVector(4)", {
        case FunCall(Map(Lambda(_, FunCall(uf: UserFun, _))), arg) =>
          asScalar() o Map(Vectorize(4)(uf)) o asVector(4) $ arg
      }),

      Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
        case FunCall(ReduceSeq(f), init, FunCall(MapSeq(g), arg)) =>
          ReduceSeq(fun( (acc, x) => f(acc, g(x))), init) $ arg
      }),

      Rule("Map(f) o Map(g) => Map(f o g)", {
        case FunCall(Map(f), FunCall(Map(g), arg)) =>
          Map(f o g) $ arg
      })
    )

  private def listAllPossibleRewritesForAllRules(lambda: Lambda): Seq[(Rule, Expr)] = {
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  private def listAllPossibleRewrites(lambda: Lambda,
                              rule: Rule): Seq[(Rule, Expr)] = {
    Context.updateContext(lambda.body, new Context)

    Expr.visitWithState(Seq[(Rule, Expr)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e) && rule.isValid(e.context)) {
        s :+ (rule, e)
      } else s
    })
  }

  private def applyRuleAt(lambda: Lambda, ruleAt: (Rule, Expr)): Lambda = {
    val rule = ruleAt._1
    val oldE = ruleAt._2
    // same as FunDecl.replace( ... )
    Lambda(lambda.params, Expr.replace(lambda.body, oldE, rule.rewrite))
  }

  def rewrite(lambda: Lambda, levels: Int = 1): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = TestRewrite.listAllPossibleRewritesForAllRules(lambda)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAt(lambda, ruleAt))

    val (g, notG) = rewritten.partition( _.isGenerable )

    if (levels == 1) {
      g
    } else {
      g ++ notG.flatMap( l => rewrite(l, levels-1))
    }
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
      // input => Join() o MapWrg(Join() o Map(MapLane(id)) o Split(2) ) o Split(2) $ input
    )

    def goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val options = TestRewrite.rewrite(f, levels = 1)
    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    assertTrue(options.nonEmpty)

    options.foreach(l => {
      println("execute: " + l)
      val (result: Array[Float], _) = Execute(128)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
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

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
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
