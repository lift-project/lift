package exploration

import arithmetic.Var
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

  private def rewrite(expr: Lambda): List[Lambda] = {
    var lambdaList = List[Lambda]()

    expr.body match {
      case call: FunCall =>
        call.f match {
          case m: Map =>

            // Map(f) => MapGlb(f)
            val mapGlb = MapGlb(m.f)

            val mapGlbLambda =
              new Lambda(expr.params, mapGlb(call.args:_*))
            lambdaList = mapGlbLambda :: lambdaList

            // Map(f) => Join() o Map(Map(f)) o Split(I)
            val splitComp = Join() o MapGlb(MapSeq(m.f)) o Split(4)

            val splitJoinLambda =
              new Lambda(expr.params, splitComp(call.args:_*))
            lambdaList = splitJoinLambda :: lambdaList

            m.f.body match {
              case callInner: FunCall =>
                callInner.f match {
                  case uf: UserFun =>

                    // Map(f) => asScalar() o Map(Vectorize(k)(f)) o asVector(k)
                    val comp = asScalar() o MapGlb(new Lambda(m.f.params, uf.vectorize(4)(callInner.args:_*))) o asVector(4)

                    val vectorizeLambda =
                      new Lambda(expr.params, comp(call.args:_*))
//                    lambdaList = vectorizeLambda :: lambdaList

                  case _  =>
                }
              case _=>
            }

          case l: Lambda =>
            l.body match {
              case callInner: FunCall =>
                callInner.f match {
                  case r: Reduce =>

                    // Reduce(f) => toGlobal(MapSeq(id) ReduceSeq(f)
                    val reduceSeqLambda =
                      new Lambda(l.params, ReduceSeq(r.f)(callInner.args:_*))

                    val reduce = new Lambda(expr.params, (toGlobal(MapSeq(id)) o reduceSeqLambda)(call.args:_*))

                    lambdaList = reduce :: lambdaList
                }
            }

          case cf: CompFun =>
          // Rules with multiple things on the left hand side
          // + recurse

            val funs = cf.funs

            funs.sliding(2).foreach(list => {
              list.head.body match {
                case call1: FunCall if call1.f.isInstanceOf[Join]=>

                  list.last.body match {
                    case call2: FunCall if call2.f.isInstanceOf[Split] =>

                      call1.args.head.t match {
                        case ArrayType(ArrayType(_, len), _) =>
                          if (len == call2.f.asInstanceOf[Split].chunkSize) {

                            val newList = funs.patch(funs.indexOfSlice(list), List(), 2)
                            // TODO: get rid of cf and extra lambda if just one left
                            lambdaList = new Lambda(expr.params, new CompFun(cf.params, newList:_*).apply(call.args:_*)) :: lambdaList
                          }

                        case _ =>
                      }

                    case _ =>
                  }
                case _ =>
              }
            })
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

    Type.check(f.body)

    val lambdaOptions = TestRewrite.rewrite(f)
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

    println(goldF)

    val lambdaOptions = TestRewrite.rewrite(f)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

}
