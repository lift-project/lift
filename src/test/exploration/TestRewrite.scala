package exploration

import arithmetic.Var
import ir._
import ir.ast._
import opencl.ir.ast._
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
              new Lambda(expr.params, splitComp(expr.params:_*))
            lambdaList = splitJoinLambda :: lambdaList

            m.f.body match {
              case call: FunCall =>
                call.f match {
                  case uf: UserFun =>

                    // Map(f) => asScalar() o Map(Vectorize(k)(f)) o asVector(k)
                    val comp = asScalar() o MapGlb(uf.vectorize(4)) o asVector(4)

                    val vectorizeLambda =
                      new Lambda(expr.params, comp(expr.params:_*))
                    lambdaList = vectorizeLambda :: lambdaList

                  case _  =>
                }
              case _=>
            }

          case l: Lambda =>
            l.body match {
              case call: FunCall =>
                //case ReduceCall(loopVar, Reduce(lambda), init, param) =>
                call.f match {
                  case r: Reduce =>

                    val reduceSeq = ReduceSeq(r.f)

                    // Reduce(f) => ReduceSeq(f)
                    val reduceSeqLambda =
                      new Lambda(expr.params, reduceSeq(call.args:_*))

                    lambdaList = reduceSeqLambda :: lambdaList
                }
            }

          case cf: CompFun =>
          // Rules with multiple things on the left hand side
          // + recurse

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

    println(f)
    println(lambdaOptions.head)

//    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)
//
//    lambdaOptions.foreach(l => {
//      val (result: Array[Float], _) = Execute(1, 1)(l, A)
//      assertArrayEquals(l + " failed", gold, result, 0.0f)
//    })
  }

}
