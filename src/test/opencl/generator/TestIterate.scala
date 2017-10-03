package opencl.generator

import ir.ArrayType
import ir.ast.{Get, Iterate, Join, Split, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.ir.pattern._
import opencl.ir.{Float, FloatToValue, Int, IntToValue, add, id}
import org.junit.Assert.assertArrayEquals
import org.junit.{Assume, Test}

object TestIterate extends TestWithExecutor

class TestIterate {
  private def incrementI = fun(Int, x => add(Int).apply(1, x))
  private def incrementF = fun(Float, x => add(Float).apply(1f, x))

  @Test
  def IterateAfterGet(): Unit = {
    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    def f = fun(
      ArrayType(Int, 128),
      ArrayType(Int, 128),
      (v, w) =>
        Join() o MapGlb(
          Iterate(5)(MapSeq(incrementI)) o Get(0)
        ) $ Zip(Split(32) $ v, Split(32) $ w)
    )

    val V = Array.fill(128)(util.Random.nextInt(1024))
    val W = Array.fill(128)(util.Random.nextInt(1024))
    val (output, _) = Execute(4, 4)[Array[Int]](f, V, W)
    assertArrayEquals(V.map(_ + 5), output)
  }

  @Test
  def testIterateAmdBug(): Unit = {
    Assume.assumeFalse("Wrong AMD IL generated", Utils.isAmdGpu)

    val inputSize = 1
    val input = Array.fill(inputSize)(0.0f)

    val iterCount = 100

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      x => MapGlb(Iterate(iterCount)(MapSeq(incrementF))) o Split(inputSize) $ x
    )

    val (output, _) = Execute(1,1)[Array[Float]](f, input)

    assertArrayEquals(Array.fill(inputSize)(100.0f), output, 0.0f)
  }

  @Test
  def issue20(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputData.map(_+5)

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      (inArr) => {
        Join() o MapGlb(
          Iterate(5)(fun((e) => MapSeq(incrementF) $ e))
        ) o Split(1) $ inArr
      }
    )

    val f2 = fun(
      ArrayType(Float, SizeVar("N")),
      (inArr) => {
        Iterate(5)(fun((arr) =>
          MapGlb(incrementF) $ arr
        )) $ inArr
      }
    )

    val (output1, _) = Execute(inputData.length)[Array[Float]](f, inputData)
    assertArrayEquals(gold, output1, 0.0f)

    val (output2, _) = Execute(inputData.length)[Array[Float]](f2, inputData)
    assertArrayEquals(gold, output2, 0.0f)
  }

  @Test
  def iterate(): Unit = {
    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+(1*7))

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      in => Iterate(7)(MapGlb(incrementF)) $ in
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, input)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def iterateFixedSecondArg() : Unit = {
    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    val inputSize = 512
    val inputA = Array.tabulate(inputSize)(_.toFloat)
    val inputB = Array.tabulate(inputSize)(_.toFloat).reverse
    val gold = inputA.zip(inputB.map(_*5.0f)).map((t:(Float, Float)) => t match{ case (x:Float,y:Float) => x+y})

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (inA,inB) => Iterate(5)(fun( (va) =>
        fun( (vb) =>
          MapWrg(add) $ Zip(va,vb)
        ) $ inB
      )) $ inA
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, inputA, inputB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def iterateLocalOnly(): Unit = {
    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+1).map(_+1).map(_+1).map(_+1).map(_+1)

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join() o MapWrg(toGlobal(MapLcl(id)) o
        Iterate(5)( MapLcl(incrementF)) o
        toLocal(MapLcl(id))) o Split(16) $ in
    )

    val f_nested = fun(
      ArrayType(Float, SizeVar("N")),
      in => fun(x1 => Join()(MapWrg(
        fun(x2 =>
          fun(x3 =>
            fun(x4 => toGlobal(MapLcl(id))(x4))(
              Iterate(5)(fun(x5 => MapLcl(incrementF)(x5)))(x3)))(
            toLocal(MapLcl(id))(x2)))
      )(x1)))(Split(16)(in))
    )

    val f_nested2 = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        fun(x3 =>
          fun(x4 => toGlobal(MapLcl(id))(x4))(
            Iterate(5)(fun(x5 => MapLcl(incrementF)(x5)))(x3)))(
          toLocal(MapLcl(id))(x2)))
      )(Split(16)(in)))
    )

    val f_nested3 = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        fun(x4 => toGlobal(MapLcl(id))(x4))(
          Iterate(5)(fun(x5 => MapLcl(incrementF)(x5)))(
            toLocal(MapLcl(id))(x2)))
      ))(Split(16)(in)))
    )

    val f_nested4 = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        toGlobal(MapLcl(id))(
          Iterate(5)(fun(x5 => MapLcl(incrementF)(x5)))(
            toLocal(MapLcl(id))(x2)))
      ))(Split(16)(in)))
    )

    val f_full = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x0 =>
        toGlobal(MapLcl(id))(
          Iterate(5)(fun(x1 => MapLcl(incrementF)(x1)))(
            toLocal(MapLcl(id))(x0)))
      ))(Split(16)(in)))
    )

    val (output1, _) = Execute(inputSize)[Array[Float]](f, input)
    assertArrayEquals(gold, output1, 0.0f)
    val (output2, _) = Execute(inputSize)[Array[Float]](f_nested, input)
    assertArrayEquals(gold, output2, 0.0f)
    val (output3, _) = Execute(inputSize)[Array[Float]](f_nested2, input)
    assertArrayEquals(gold, output3, 0.0f)
    val (output4, _) = Execute(inputSize)[Array[Float]](f_nested3, input)
    assertArrayEquals(gold, output4, 0.0f)
    val (output5, _) = Execute(inputSize)[Array[Float]](f_nested4, input)
    assertArrayEquals(gold, output5, 0.0f)
    val (output6, _) = Execute(inputSize)[Array[Float]](f_full, input)
    assertArrayEquals(gold, output6, 0.0f)
  }
}
