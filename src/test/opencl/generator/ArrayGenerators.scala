package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast.{Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayFromExpr, ArrayFromGenerator, ArrayFromUserFunGenerator, ArrayFromValue, Split, UserFun, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, TestWithExecutor}
import core.generator.GenericAST.ArithExpression
import opencl.ir.pattern.{MapGlb, MapSeq, toGlobal}
import opencl.ir.{Float, FloatToValue, Int, IntToValue, add, id}
import org.junit.Assert.assertArrayEquals
import org.junit.Test
import opencl.ir._

object ArrayGenerators extends TestWithExecutor

class ArrayGenerators {
  @Test
  def arrayFromValue(): Unit = {
    val input = Array.fill(128)(util.Random.nextFloat())

    val at = ArrayTypeWSWC(Float, SizeVar("N"))
    val f = fun(at,
      input => MapGlb(add) $ Zip(input, ArrayFromValue(1f, at))
    )

    val (output, _) = Execute(input.length)[Array[Float]](f, input)
    val gold = input.map(_ + 1)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def sparseArrayFromValue(): Unit = {
    val size = 100
    val capacity = 128

    val f = fun(
      MapGlb(toGlobal(fun(x => add(Int).apply(22, x)))) $
      ArrayFromValue(20, ArrayTypeWSWC(Int, size, capacity))
    )

    val (output, _) = Execute(capacity)[Array[Int]](f)
    assertArrayEquals(Array.fill(size)(42), output.slice(0, size))
  }

  @Test
  def arrayFromGenerator(): Unit = {
    val input = Array.fill(128)(util.Random.nextFloat())

    val at = ArrayTypeWSWC(Float, SizeVar("N"))
    val f = fun(at,
      input => MapGlb(add) $ Zip(input, ArrayFromGenerator( (i, _) => ArithExpression(i), at))
    )

    val (output, _) = Execute(input.length)[Array[Float]](f, input)
    val gold = (input, Array.tabulate(input.length)( i => i )).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def arrayFromUserFunGenerator(): Unit = {
    val input = Array.fill(128)(util.Random.nextFloat())

    val at = ArrayTypeWSWC(Float, SizeVar("N"))
    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    val f = fun(at,
      input => MapGlb(add) $ Zip(input, ArrayFromUserFunGenerator(idxF, at))
    )

    val (output, _) = Execute(input.length)[Array[Float]](f, input)
    val gold = (input, Array.tabulate(input.length)( i => i )).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def arrayFromUserFunGenerator2(): Unit = {
    val input = Array.fill(128)(util.Random.nextInt())

    val at = ArrayTypeWSWC(Int, SizeVar("N"))
    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    val f = fun(at,
      _ => // the input is not used but helps size inference
        ArrayFromUserFunGenerator(idxF, at) :>>
          Split(64) :>>
          toGlobal(MapGlb(MapSeq(id(Int))))
    )

    val (output, _) = Execute(input.length)[Array[Int]](f, input)
    val gold = Array.tabulate(input.length)( i => i )

    assertArrayEquals(gold, output)
  }

  @Test
  def arrayFromUserFunGenerator2D(): Unit = {
    val m = 128
    val n = 8

    val input = Array.fill(m, n)(util.Random.nextInt())

    val M = SizeVar("M")
    val N = SizeVar("N")

    val at = ArrayTypeWSWC(ArrayTypeWSWC(Int, N), M)

    val idxF = UserFun("idxF", Array("i", "j", "m", "n"), "{ return i+j; }",
      Seq(Int, Int, Int, Int), Int)

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Int, N), M),
      _ => // the input is not used but helps size inference
        Array2DFromUserFunGenerator(idxF, at) :>>
          toGlobal(MapGlb(MapSeq(id(Int))))
    )

    val (output, _) = Execute(input.length)[Array[Int]](f, input)
    val gold = Array.tabulate(m, n)( (i, j) => i+j ).flatten

    assertArrayEquals(gold, output)
  }

  @Test
  def arrayFromUserFunGenerator3D(): Unit = {
    val m = 128
    val n = 8
    val o = 4

    val input = Array.fill(m, n, o)(util.Random.nextInt())

    val M = SizeVar("M")
    val N = SizeVar("N")
    val O = SizeVar("O")

    val at = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O), N), M)

    val idxF = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ return i+j+k; }",
      Seq(Int, Int, Int, Int, Int, Int), Int)

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O), N), M),
      _ => // the input is not used but helps size inference
        Array3DFromUserFunGenerator(idxF, at) :>>
          toGlobal(MapGlb(MapSeq(MapSeq(id(Int)))))
    )

    val (output, _) = Execute(input.length)[Array[Int]](f, input)
    val gold = Array.tabulate(m, n, o)( (i, j, k) => i+j+k ).flatten.flatten

    assertArrayEquals(gold, output)
  }

  @Test
  def arrayFromExpressionsOneExpr1D(): Unit = {

      val size = 10
      val N = SizeVar("N")

      val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
      val gold = values

      def exprTest = fun(
        ArrayTypeWSWC(Float,N),
        (input) => {

          toGlobal(MapSeq(tf_id)) $ Zip(MapSeq(id) $ ArrayFromExpr(input.at(0)), MapSeq(id) $ ArrayFromExpr(input.at(N-1)))

        })

      println(Compile(exprTest))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](exprTest, values)
    //  val (output, _) = Execute(2,2)[Vector[(Float, Float)]](exprTest, values)

      //    assertArrayEquals(gold, output, 0.1f)

  }

}
