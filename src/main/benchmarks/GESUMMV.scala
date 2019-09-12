package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.executor.Utils
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class GESUMMV (override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("GESUMMV", Seq(4096, 4096), f, 0.0f) {

  override def runScala(inputs: Any*): Array[Float] = {
    val A = inputs(0).asInstanceOf[Array[Array[Float]]]
    val B = inputs(1).asInstanceOf[Array[Array[Float]]]
    val x = inputs(2).asInstanceOf[Array[Float]]
    val alpha = inputs(3).asInstanceOf[Float]
    val beta = inputs(4).asInstanceOf[Float]

    val tmp1Gold = Utils.matrixVector(A, x, alpha)
    val tmp2Gold = Utils.matrixVector(B, x, beta)
    val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

    yGold
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)

    val alpha = 2.0f
    val beta = 1.5f
    val x = Array.fill(inputSizeM)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(inputSizeN, inputSizeM)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(inputSizeN, inputSizeM)(util.Random.nextInt(5).toFloat)

    val values = Seq(A, B, x, alpha, beta)

    values
  }
}

object GESUMMV {

  val N = SizeVar("N")
  val K = SizeVar("M")

  val f = UserFun("f", Array("acc", "a", "b", "x"),
    "{ Tuple t = { acc._0 + a * x, acc._1 + b * x };" +
      "return t; }",
    Seq(TupleType(Float, Float), Float, Float, Float), TupleType(Float, Float))

  val g = UserFun("g", Array("alpha", "a", "beta", "b"),
    "{ return alpha * a + beta * b; }",
    Seq(Float, Float, Float, Float), Float)


  val fused = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
    Float, Float,
    (A, B, x, alpha, beta) =>
      Zip(A, B) :>> MapGlb(\( p => {
        val aRow = p._0
        val bRow = p._1
        Zip(aRow, bRow, x) :>>
          ReduceSeq(\( (acc, p) => {
            val a = p._0
            val b = p._1
            val x = p._2
            f(acc, a, b, x)
          }), Value("{0.0f, 0.0f}", TupleType(Float, Float))) :>>
          MapSeq(\( p => { g(alpha, p._0, beta, p._1) })) :>> toGlobal(MapSeq(id))
      }))
  )

  val simpleUserFun = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
    Float, Float,
    (A, B, x, alpha, beta) =>
      Zip(A, B) :>> MapGlb(\( p => {
        val aRow = p._0
        val bRow = p._1
        Zip(aRow, bRow, x) :>>
          ReduceSeq(\( (acc, p) => {
            val a = p._0
            val b = p._1
            val x = p._2

            idFF $ Tuple(add(acc._0, mult(a, x)), add(acc._1, mult(b, x)))

          }), Value("{0.0f, 0.0f}", TupleType(Float, Float))) :>>
          MapSeq(\( p =>  add(mult(alpha, p._0), mult(beta, p._1)) )) :>>
          toGlobal(MapSeq(id))
      }))
  )

  val h = UserFun("h", Array("a", "b"), "{ Tuple t = {a._0 + b._0, a._1 + b._1}; return t; }",
    Seq(TupleType(Float, Float), TupleType(Float, Float)),
    TupleType(Float, Float))

  val stride = 128

  val fusedOptimised = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
    Float, Float,
    (A, B, x, alpha, beta) =>
      MapWrg(fun(matrices =>
        toGlobal(MapLcl(fun(partials => g(alpha, partials._0, beta, partials._1)))) o
          MapSeq(toLocal(idFF)) o
          ReduceSeq(h, Value("{0.0f, 0.0f}", TupleType(Float, Float))) o
          Join() o
          MapLcl(MapSeq(toLocal(idFF)) o
            ReduceSeq(fun((acc, next) =>
              fun(inPrivate =>
                f(acc, Get(inPrivate, 1), Get(inPrivate,2), Get(inPrivate,0))
              ) o toPrivate(fun(next => Tuple(id(Get(next, 0)), id(Get(next,1)), id(Get(next,2))))) $ next
            ), Value("{0.0f, 0.0f}", TupleType(Float, Float)))
          ) o
          Split(K/^stride) o
          ReorderStride(stride) $
          Zip(x, Get(matrices, 0), Get(matrices, 1))
      )) $ Zip(A, B)
  )

  def apply() = new GESUMMV(Seq(
    ("fused", Array[Lambda](fused)),
    ("fusedOptimised", Array[Lambda](fusedOptimised)))
  )

  def main(args: Array[String]): Unit = {
    GESUMMV().run(args)
  }
}
