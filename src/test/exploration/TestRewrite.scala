package exploration

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

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

      Rule("Transpose() o Transpose() => id", {
        case FunCall(Transpose(), FunCall(Transpose(), arg)) => arg
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

      Rule("Map(Reduce(f)) => Reduce(Map(f)) o Transpose()", {
        case FunCall(Map(Lambda(lambdaParams, FunCall(Reduce(f), init: Value, arg))), mapArg)
          if lambdaParams.head eq arg
        =>
          val newInit = Value(init.value, ArrayType(init.t, Type.getLength(mapArg.t)))
          Reduce(fun((acc, c) => Map(fun(x => f(Get(x, 0), Get(x, 1)))) $ Zip(acc, c)),
            newInit) o Transpose() $ mapArg
      }),

      Rule("Map(fun(a => Map() $ Zip(..., a, ...)) $ A => Transpose() o Map(Map(fun(a => ))) $ Zip(..., Transpose() $ A, ...) ", {
        case FunCall(Map(
          Lambda(outerLambdaParam, FunCall(Map(
            Lambda(innerLambdaParam, expr
            )), FunCall(Zip(_), zipArgs @ _*))
          )), arg)
          if zipArgs.contains(outerLambdaParam.head)
        =>
          // Find all Get patterns that refer to the an element from the zipped array
          // and have to be replaced in expr
          val gets = Expr.visitWithState(List[FunCall]())(expr, (e, s) => {
            e match {
              case get @ FunCall(Get(_), getParam) if getParam eq innerLambdaParam.head  => get :: s
              case _ => s
            }
          })

          // Find which Get pattern corresponds to the component containing an element of 'a'
          val zipToReplace = zipArgs.zipWithIndex.find(e => e._1 eq outerLambdaParam.head).get

          // Create the new Get patterns with a new parameter
          val newParam = Param()
          val getPairs =  gets zip gets.map(get => Get(newParam, get.f.asInstanceOf[Get].n))

          // Separate the Get pattern containing an element of 'a', as it will now refer
          // to the component containing an element of 'A'
          val (one, two) = getPairs.partition(_._1.f.asInstanceOf[Get].n == zipToReplace._2)

          // Replace most of the old Get patterns with new ones
          val newExpr = two.foldRight(expr)((get, e) => Expr.replace(e, get._1, get._2))

          // Create a new parameter for an element of 'a' and replace for the Get referring
          // an element of 'a'
          val secondNewParam = Param()
          val finalNewExpr = Expr.replace(newExpr, one.head._1, secondNewParam)

          // Create the arguments for the zip, replacing 'a' with 'Transpose() $ A'
          val newZipArgs = zipArgs.updated(zipToReplace._2, Transpose() $ arg)

          // Construct the final expression
          val lambda = Lambda(Array(secondNewParam), finalNewExpr)
          Transpose() o Map(Lambda(Array(newParam), Map(lambda) $ one.head._2)) $ Zip(newZipArgs:_*)
      }),

      Rule("Map(f o g) => Map(f) o Map(g)", {
        case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2)))), arg)
          if p2.contains({ case a => a eq p1.head})
        =>
          Map(fun1) o Map(Lambda(p1, fun2(p2))) $ arg
        case FunCall(Map(Lambda(p1, FunCall(Reduce(fun1), init, FunCall(fun2, p2)))), arg)
          if p2.contains({ case a => a eq p1.head})
        =>
          Map(Reduce(fun1, init)) o Map(Lambda(p1, fun2(p2))) $ arg
      }),

      Rule("Reduce(f) => ReduceSeq(f)", {
        case FunCall(Reduce(f), init, arg) =>

          // Construct id functions using the type of init
          var idFunction: FunDecl = id
          var idFunction2: FunDecl = id
          Type.visit(init.t, t => if(t.isInstanceOf[ArrayType]) {
            idFunction = Map(idFunction)
            idFunction2 = Map(idFunction2)
          }, t => Unit)

          val newInit = if (init.isInstanceOf[Value]) idFunction $ init else init

          toGlobal(MapSeq(idFunction2)) o ReduceSeq(f, newInit) $ arg
      }),

      Rule("Reduce(f) => Reduce(f) o PartRed(f)", {
        case FunCall(Reduce(f), init, arg) =>
          Reduce(f, init) o PartRed(f, init) $ arg
      }),

      Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
        case FunCall(PartRed(f), init, arg) =>
          Join() o Map(PartRed(f, init)) o Split(4) $ arg
      }),

      Rule("Map(uf) => asScalar() o MapGlb(Vectorize(4)(uf)) o asVector(4)", {
        case FunCall(Map(Lambda(_, FunCall(uf: UserFun, _))), arg) =>
          asScalar() o Map(Vectorize(4)(uf)) o asVector(4) $ arg
      }),

      Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
        case FunCall(ReduceSeq(f), init, FunCall(MapSeq(g), arg))
          if g.isGenerable
        =>
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
    Lambda(lambda.params, Expr.replace(lambda.body, oldE, rule.rewrite(oldE)))
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
  def mmReuseA(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    TypeChecker.check(f.body)

    val splitJoin = f.body match {
      case FunCall(Map(Lambda(_, call @ FunCall(_, _))), _) => call
    }

    val splitJoinRewrite = TestRewrite.rules(16).rewrite
    assertTrue(splitJoinRewrite.isDefinedAt(splitJoin))

    val f1 = FunDecl.replace(f, splitJoin, splitJoinRewrite(splitJoin))
    TypeChecker.check(f1.body)

    val mapFission = f1.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, call)), _)))), _) => call
    }

    val mapFissionRewrite = TestRewrite.rules(19).rewrite
    assertTrue(mapFissionRewrite.isDefinedAt(mapFission))

    val f2 = FunDecl.replace(f1, mapFission, mapFissionRewrite(mapFission))
    TypeChecker.check(f2.body)

    val mapReduceInterchange = f2.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, call)), _)))), _) => call
    }

    val mapReduceInterchangeRewrite = TestRewrite.rules(17).rewrite
    assertTrue(mapReduceInterchangeRewrite.isDefinedAt(mapReduceInterchange))

    val f3 = FunDecl.replace(f2, mapReduceInterchange, mapReduceInterchangeRewrite(mapReduceInterchange))
    TypeChecker.check(f3.body)

    val mapMapTranspose = f3.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(_, call)))), _)))), _) => call
    }

    val mapMapTransposeRewrite = TestRewrite.rules(18).rewrite
    assertTrue(mapMapTransposeRewrite.isDefinedAt(mapMapTranspose))

    val f4 = FunDecl.replace(f3, mapMapTranspose, mapMapTransposeRewrite(mapMapTranspose))
    TypeChecker.check(f4.body)

    val transposeTranspose = f4.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, call))), _)))), _) => call
    }

    val transposeTransposeRewrite = TestRewrite.rules(7).rewrite
    assertTrue(transposeTransposeRewrite.isDefinedAt(transposeTranspose))

    val f5 = FunDecl.replace(f4, transposeTranspose, transposeTransposeRewrite(transposeTranspose))
    TypeChecker.check(f5.body)


    val mapToMapSeq = f5.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Map(Lambda(_, call)), _)))), _)))), _) => call
    }

    val mapToMapSeqRewrite = TestRewrite.rules(10).rewrite
    assertTrue(mapToMapSeqRewrite.isDefinedAt(mapToMapSeq))

    val f6 = FunDecl.replace(f5, mapToMapSeq, mapToMapSeqRewrite(mapToMapSeq))
    TypeChecker.check(f6.body)

    val mapToMapSeq2 = f6.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, call)), _, _))), _)))), _) => call
    }

    assertTrue(mapToMapSeqRewrite.isDefinedAt(mapToMapSeq2))

    val f7 = FunDecl.replace(f6, mapToMapSeq2, mapToMapSeqRewrite(mapToMapSeq2))
    TypeChecker.check(f7.body)

    val mapToMapSeq3 = f7.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, call))), _)))), _) => call
    }

    assertTrue(mapToMapSeqRewrite.isDefinedAt(mapToMapSeq3))

    val f8 = FunDecl.replace(f7, mapToMapSeq3, mapToMapSeqRewrite(mapToMapSeq3))
    TypeChecker.check(f8.body)

    val reduceToReduceSeq = f8.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, call)), _)))), _) => call
    }

    val reduceToReduceSeqRewrite = TestRewrite.rules(20).rewrite
    assertTrue(reduceToReduceSeqRewrite.isDefinedAt(reduceToReduceSeq))

    val f9 = FunDecl.replace(f8, reduceToReduceSeq, reduceToReduceSeqRewrite(reduceToReduceSeq))
    TypeChecker.check(f9.body)

    val fusion = f9.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, call))), _)))), _) => call
    }

    val fusionRewrite = TestRewrite.rules(24).rewrite
    assertTrue(fusionRewrite.isDefinedAt(fusion))

    val f10 = FunDecl.replace(f9, fusion, fusionRewrite(fusion))
    TypeChecker.check(f10.body)

    println(f10)
  }

  @Test
  def mapFission(): Unit = {
    val N = Var("N")

    val f = fun(
      ArrayType(Float, N),
      input => Map(id o id) $ input
    )

    assertTrue(TestRewrite.rules(19).rewrite.isDefinedAt(f.body))
    println(Lambda(f.params, TestRewrite.rules(19).rewrite(f.body)))

    val M = Var("M")

    val g = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Map(fun(x => Reduce(add, 0.0f) o Map(id) $ Zip(x, x))) $ input
    )

    println(g)

    assertTrue(TestRewrite.rules(19).rewrite.isDefinedAt(g.body))
    println(Lambda(g.params, TestRewrite.rules(19).rewrite(g.body)))
  }

  @Test
  def transposeTransposeId(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Transpose() o Transpose() $ input
    )

    assertTrue(TestRewrite.rules(7).rewrite.isDefinedAt(f.body))
    assertSame(f.params.head, TestRewrite.rules(7).rewrite(f.body))
  }

  @Test
  def mapReduceInterchange(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    assertTrue(TestRewrite.rules(17).rewrite.isDefinedAt(f.body))
  }

  @Test
  def mapMapTransposeWithZip(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
                ArrayType(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(x => add(Get(x, 0), Get(x, 1)))) $ Zip(in2, x))) $ in1
    )

    assertTrue(TestRewrite.rules(18).rewrite.isDefinedAt(f.body))
    println(TestRewrite.rules(18).rewrite(f.body))
  }

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
