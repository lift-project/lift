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

//  private def rewriteOld(expr: Lambda): List[Lambda] = {
//
//    var replacements = List[(Seq[Lambda], Seq[Lambda])]()
//
//    case class Rule(desc: String, fct: PartialFunction[List[Lambda],(Seq[Lambda], Seq[Lambda])]) {
//      def apply(expr: List[Lambda]): Option[(Seq[Lambda], Seq[Lambda])] = {
//        if(fct.isDefinedAt(expr)) {
//          //System.out.println(s"Rule '$desc' is applicable to '$expr'")
//          Some(fct.apply(expr))
//        } else None
//      }
//    }
//
//    val rules:Seq[Rule] = Seq[Rule](
//      // === SIMPLIFICATION RULES ===
//      Rule("Iterate(0, _) => Epsilon", {
//        case (l @ Pattern(Iterate(n,_))) :: xs if n.eval == 0 => (Seq(l), Seq()) }),
//
//      Rule("Iterate(1, x) => x", {
//        case (l @ Pattern(Iterate(n,Lambda(_, f:FunCall)))) :: xs if n.eval == 1 => (Seq(l), Seq()) }),
//
//      Rule("Epsilon() o x => x", {
//        case Pattern(Epsilon()) :: (l:Lambda) :: xs => (Seq(l), Seq()) }),
//
//      Rule("x o Epsilon() => x", {
//        case (l:Lambda) :: Pattern(Epsilon()) :: xs => (Seq(l), Seq()) }),
//
//      Rule("Map(Epsilon()) => Epsilon()", {
//        case (l @ Lambda(_, FunCall(Map(Lambda(_, FunCall(Epsilon(), _))), _))) :: xs => (Seq(l), Seq()) }),
//
//      Rule("joinVec o splitVec => id", {
//        case (one @ Pattern(asScalar())) :: (two @ Pattern(asVector(_))) :: xs =>
//          (Seq(one, two), Seq())
//      }),
//
//      Rule("Join() o Split(_) => id", {
//        case (one @ Pattern(Join())) :: (two @ Pattern(Split(_))) :: xs =>
//          (Seq(one, two), Seq())
//      }),
//
//      Rule("splitVec(n) o joinVec(n) => id", {
//        case (one @ Pattern(asVector(splitVectorWidth))) :: (two @ FunCallInst(asScalar(), joinArg)) :: xs =>
//          joinArg.t match {
//            case ArrayType(VectorType(_, joinVectorWidth), _) if joinVectorWidth == splitVectorWidth =>
//                (Seq(one, two), Seq())
//            case _ => (Seq(), Seq())
//          }
//      }),
//
//      Rule("Split(n) o Join(n) => id", {
//        case (one @ Pattern(Split(splitChunkSize))) :: (two @ FunCallInst(Join(),joinArg)) :: xs =>
//          joinArg.t match {
//            case ArrayType(ArrayType(_, joinChunkSize), _) if joinChunkSize == splitChunkSize =>
//              (Seq(one, two), Seq())
//            case _ => (Seq(), Seq())
//          }
//        }),
//
//      // == NONCONTRACTING RULES ==
//
//      Rule("Map(f) => MapGlb(f)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, FunCall(MapGlb(l), args))))
//      }),
//
//      Rule("Map(f) => MapWrg(f)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, MapWrg(l)(args)))) }),
//
//      Rule("Map(f) => MapLcl(f)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, MapLcl(l)(args)))) }),
//
//      Rule("Map(f) => MapSeq(f)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, MapSeq(l)(args)))) }),
//
//      Rule("Map(f) => MapLane(f)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, MapLane(l)(args)))) }),
//
//      Rule("Map(f) => MapWarp(f)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, MapWarp(l)(args)))) }),
//
////      Rule("Map(f) => MapWarp(f)", {
////        case call @ FunCall(Map(l), args) =>
////          (call, MapWarp(l)(args)) }),
//
//      Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
//        case (outer @ Lambda(params, FunCall(Map(l), args))) :: xs =>
//          (Seq(outer), Seq(Join(), MapGlb(MapSeq(l)), Split(4)))
//      }),
//
////      Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
////        case call @ FunCall(Map(l), args) =>
////          (call, Join() $ MapGlb(MapSeq(l)) $ Split(4) $ args )
////      }),
//
//      Rule("Reduce(f) => toGlobal(MapSeq(id)) ReduceSeq(f)", {
//        case (outer @ Lambda(params, FunCall(Lambda(innerParams, FunCall(Reduce(l), innerArgs @ _*)) , arg))) :: xs =>
//          (Seq(outer), Seq(Lambda(params, FunCall(toGlobal(MapSeq(id)) o Lambda(innerParams, ReduceSeq(l)(innerArgs:_*)), arg))))
//        }),
//
//      Rule("Map(f) => asScalar() o MapGlb(f.vectorize(4)) o asVector(4)", {
//        case (outer @ Lambda(params, FunCall(Map(Lambda(innerParams, FunCall(uf: UserFun, innerArg))), arg))) :: xs =>
//          val vectorWidth = 4
//          (Seq(outer), Seq(asScalar(), MapGlb(uf.vectorize(vectorWidth)), asVector(vectorWidth)))
//        }),
//
//      Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
//        case (reduce @ Lambda(reduceParams, FunCall(ReduceSeq(Lambda(accNew, FunCall(redFun, redFunArgs @ _*))), reduceArgs @ _*))) ::
//          (map @ Lambda(_, FunCall(MapSeq(mapLambda), _))) :: xs =>
//          val newReduceFunArgs = redFunArgs.map(Expr.replace(_, accNew(1), mapLambda(accNew(1))))
//          val replacement = Seq(Lambda(reduceParams, ReduceSeq(Lambda(accNew, redFun(newReduceFunArgs: _*)))(reduceArgs:_*)))
//          (Seq(reduce, map), replacement)
//        })
//
////      Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
////        case (all @ FunCall(ReduceSeq(f @ Lambda(_, _)), init, FunCall(MapSeq(g @ Lambda(_, _)), arg))) =>
////          (all, ReduceSeq(fun( (acc, x) => f(acc, g(x))))(init, arg))
////
////
////        case (reduce @ Lambda(reduceParams, FunCall(ReduceSeq(Lambda(accNew, FunCall(redFun, redFunArgs @ _*))), reduceArgs @ _*))) ::
////             (map @ Lambda(_, FunCall(MapSeq(mapLambda), _))) :: xs =>
////          val newReduceFunArgs = redFunArgs.map(Expr.replace(_, accNew(1), mapLambda(accNew(1))))
////          val replacement = Seq(Lambda(reduceParams, ReduceSeq(Lambda(accNew, redFun(newReduceFunArgs: _*)))(reduceArgs:_*)))
////          (Seq(reduce, map), replacement)
////      })
//
//      // === CONTRACTING RULES ===
//      /*Rule("Map(M) o Map(N) => Map(M o N)", {
//        case Lambda(args1,FunCall(Map(l1),cargs1@_*)) :: Lambda(args2,FunCall(Map(l2),cargs2@_*)) :: xs
//        if args1 sameElements cargs1.toArray =>
//          val replacement = Seq(Lambda(args2, FunCall(Map(l1 o l2),cargs2)))
//          xs
//        })*/
//    )
//
//    println(s">>> Rewriting $expr")
//    val lambdas: List[Lambda] = expr match {
////      case Lambda(_, FunCall(CompFun(functions@_*), _)) => functions.toList
//      case l@Lambda(_, FunCall(_, _)) => List(l)
//      case _ => List()
//    }
//
//    def visit(expr:Lambda, lambdas: List[Lambda], pos: Int = 0) {
//      if(lambdas.nonEmpty) {
//        rules.foreach(rule => rule(lambdas) match{
//          case Some(newReplacement) => replacements = newReplacement :: replacements
//          case None =>
//        })
//        visit(expr, lambdas.tail, pos+1)
//      }
//    }
//
//    // Type alias for a replacement list.
//    // A replacement set represents one transformation: it consumes n patterns from s and emits m replacements
//    type ReplacementSet = (Int, Seq[Lambda], Seq[Lambda])
//    type RewriteList = List[ReplacementSet]
//
//    // List all the replacements applicable to the current list, for all positions.
//    def listReplacements(lambdas: List[Lambda], pos: Int = 0, replacements: RewriteList = List.empty): RewriteList = {
//      if(lambdas.nonEmpty) {
//        // The list of transformations which are applicable to the current cursor
//        var current: RewriteList = List.empty
//        rules.foreach(rule => rule(lambdas) match{
//          case Some(out) => current = (pos, out._1, out._2) :: current
//          case None =>
//        })
//        // move the cursor by 1 and recurse
//        listReplacements(lambdas.tail, pos+1, current ++ replacements)
//      } else replacements
//    }
//
//    // Detect a collision betseen replacement sets
//    def testChangeCollision(a:ReplacementSet, b:ReplacementSet): Boolean = {
//      val a_start = a._1
//      val b_start = b._1
//      val a_end = a_start + a._2.length
//      a_start <= b_start && b_start <= a_end
//    }
//
//    var counter = 0
//
//    // list all the possible re-write for a given depth in the tree
//    def listAllRewrite(expr: Lambda, depth: Int): Unit = {
//      // test for max depth
//      counter = counter +1
//      if(depth == 0){
//        println(s"Max depth reached. Not expanding $expr")
//        return
//      }
//
//      // extract the list of lambdas
//      val lambdas: List[Lambda] = expr match {
////        case Lambda(_, FunCall(CompFun(functions@_*), _)) => functions.toList
//        case l@Lambda(_, FunCall(_, _)) => List(l)
//        case _ => List()
//      }
//
//      // List all the possible rewrite with a single traversal
//      val list:RewriteList = listReplacements(lambdas)
//      println(s"Found ${list.length} possible re-write")
//
//      // apply all the change set combinations, if they do not collide
//      // for all elements in the transformation powerset
//      /*for {
//        len <- 1 to list.length
//        changecomb <- list combinations len
//        if changecomb.nonEmpty
//      } yield {
//        //val collide = changecomb.combinations(2).forall((x,y) => testChangeCollision(x,y) == false)
//        // look for collisions
//        println(changecomb.combinations(2).toList)
//      }*/
//
//      list.foreach((x) => {
//        val newLambda = FunDecl.replace(expr, x._2, x._3)
//        System.out.println(s"New lambda: $newLambda")
//        listAllRewrite(newLambda, depth-1)
//      })
//    }
//
//    listAllRewrite(expr, 1)
//
//    visit(expr, lambdas)
//
//    println(s"Total of $counter new lambdas generated")
//
//    val result = replacements.map(pair => {
//      val toReplace = pair._1
//      val replacer = pair._2
//
//      FunDecl.replace(expr, toReplace, replacer)
//    })
//
//    result
//  }

  case class Rule(desc: String,
                  rewrite: PartialFunction[Expr, Expr])

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

      Rule("Map(f) => MapGlb(f)", {
        case FunCall(Map(f), arg) => MapGlb(f)(arg)
      }),

      Rule("Map(f) => MapWrg(f)", {
        case FunCall(Map(f), arg) => MapWrg(f)(arg)
      }),

      Rule("Map(f) => MapLcl(f)", {
        case FunCall(Map(f), arg) => MapLcl(f)(arg)
      }),

      Rule("Map(f) => MapSeq(f)", {
        case FunCall(Map(f), arg) => MapSeq(f)(arg)
      }),

      Rule("Map(f) => MapLane(f)", {
        case FunCall(Map(f), arg) => MapLane(f)(arg)
      }),

      Rule("Map(f) => MapWarp(f)", {
        case FunCall(Map(f), arg) => MapWarp(f)(arg)
      }),

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

  def listAllPossibleRewritesForAllRules(lambda: Lambda): Seq[(Int, Rule)] = {
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  def listAllPossibleRewrites(lambda: Lambda,
                              rule: Rule): Seq[(Int, Rule)] = {
    TypeChecker.check(lambda.body)

    var option = 0
    Expr.visitWithState(Seq[(Int, Rule)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e)) {
        val newS = s :+ (option, rule)
        option = option + 1
        newS
      } else s
    })
  }

  def replace(where: Int) = {
    var where_ = where
    new {
      def apply(e: Expr, rule: PartialFunction[Expr, Expr]): Expr = {
        if (rule.isDefinedAt(e)) {
          if (where_ == 0)
            return rule(e)
          else
            where_ = where_ - 1
        }

        e match {
          case call: FunCall =>
            val newArgs = call.args.map((arg) => apply(arg, rule))

            val newCall = call.f match {
              case fp: FPattern =>
                // Try to do the replacement in the body
                val replaced = apply(fp.f.body, rule)

                // If replacement didn't occur return fp
                // else instantiate a new pattern with the updated lambda
                if (fp.f.body.eq(replaced))
                  fp
                else
                  fp.copy(Lambda(fp.f.params, replaced))

              case l: Lambda =>
                // Try to do the replacement in the body
                val replaced = apply(l.body, rule)

                // If replacement didn't occur return l
                // else instantiate the updated lambda
                if (l.body.eq(replaced))
                  l
                else
                  Lambda(l.params, replaced)

              case other => other
            }

            if (!newCall.eq(call.f) || newArgs != call.args)
              FunCall(newCall, newArgs: _*)
            // Instantiate a new FunCall if anything has changed

            else
              e // Otherwise return the same FunCall object

          case _ => e
        }
      }
    }
  }

  def applyRuleAt(lambda: Lambda, ruleAt: (Int, Rule)): Lambda = {
    val where = ruleAt._1
    val rule  = ruleAt._2
    Lambda(lambda.params, replace(where)(lambda.body, rule.rewrite))
  }

  private def rewrite(lambda: Lambda): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = TestRewrite.listAllPossibleRewritesForAllRules(lambda)
    allRulesAt.map(ruleAt => applyRuleAt(lambda, ruleAt))

//    // 1. try to apply every rule once
//    // 2. filter out the once which didn't change anything
//    rules.map(rule =>
//      Lambda(lambda.params, Expr.replace(lambda.body, rule.rewrite))
//    ).filterNot( l => l.body.eq(lambda.body) )
  }
}

class TestRewrite {
  val N = Var("N")
  val A = Array.fill[Float](128)(0.5f)

  @Test
  def simpleMapTest(): Unit = {

    def f = fun(
      ArrayType(Float, N),
      input => Map(id) o Map(id) $ input
    )

    def goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val all = TestRewrite.listAllPossibleRewritesForAllRules(f)

    val f2 = TestRewrite.applyRuleAt(f, all(11))

    val lambdaOptions = TestRewrite.rewrite(f)
    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
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
