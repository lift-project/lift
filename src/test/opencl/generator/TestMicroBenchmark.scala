package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir.pattern._
import opencl.ir._
import org.junit._
import org.junit.Assert._
import rewriting.InferNDRange

object BenchmarkUserFun {

  val N = SizeVar("N")

  def spliceInUserFun(code: String, uf: UserFun): String = {
    val lines = code.split("\n")

    val kernelStartLine = lines.indexWhere(_.contains("benchmark" + uf.name))

    val block = OpenCLAST.Block()
    if (uf.tupleTypes.length == 1)
      block += OpenCLAST.TupleAlias(uf.tupleTypes.head, "Tuple")
    else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
      // TODO: think about this one ...
      block += OpenCLAST.TupleAlias(x, s"Tuple$i")
    })

    block += OpenCLAST.OpenCLCode(uf.body)
      val userFunDefinition = OpenCLAST.Function(
        name = uf.name,
        ret = uf.outT,
        params = (uf.inTs, uf.paramNames).
          zipped.map((t, n) => OpenCLAST.ParamDecl(n, t)).toList,
        body = block)

    val printeduserFun = OpenCLPrinter()(userFunDefinition)

    val beginLines = lines.slice(0, kernelStartLine-1)
    val endLines = lines.slice(kernelStartLine, lines.size)

    val kernelCode = beginLines.mkString("\n") + s"\n\n$printeduserFun\n\n" + endLines.mkString("\n")

    kernelCode
  }

  def apply(uf: UserFun, count: Int): Lambda = {

    val inputTypes = createInputTypes(uf)

    val params = inputTypes.map(t => {
      val p = Param()
      p.t = t
      p
    })

    val wrapperFun = createWrapperFun(uf, count)

    Lambda(params.toArray, MapGlb(wrapperFun) $ Zip(params:_*))
  }

  def createInputTypes(uf: UserFun): Seq[Type] = {
    uf.inTs.map(ArrayType(_, N)) :+ ArrayType(Int, N)
  }

  def createWrapperFun(uf: UserFun, count: Int): UserFun = {
    val args = uf.paramNames :+ "testValue"
    val inputTypes = uf.inTs :+ Int

    val outputTypeName = OpenCLPrinter.toString(uf.outT)

    val increment = (uf.inTs, uf.paramNames).zipped.map((t, name) => createIncrement(t, name)).mkString(" ")

    val body =
      s"""{
        |  int count = $count;
        |  $outputTypeName my_output;
        |
        |  for (int i = 0; i < count; i++) {
        |    if (testValue <= 10 + i) {
        |      my_output = ${uf.name}(${uf.paramNames.mkString(", ")});
        |    } else {
        |      $increment
        |    }
        |  }
        |
        |  return my_output;
        |}
      """.stripMargin

    UserFun("benchmark" + uf.name, args, body, inputTypes, uf.outT)
  }

  def createIncrement(t: Type, name: String): String = t match {
    case Float | Double | Int | Long => s"$name++;"
    case VectorType(basicType, n) =>
      (0 until n.eval).map(i => createIncrement(basicType, s"$name.s$i")).mkString(" ")
    case TupleType(tt@_*) =>
      tt.zipWithIndex.map(pair => createIncrement(pair._1, s"$name._${pair._2}")).mkString(" ")
    case illegalType => throw new IllegalArgumentException(illegalType.toString)
  }

}

object TestMicroBenchmark extends TestWithExecutor

class TestMicroBenchmark {

  import BenchmarkUserFun.N

  @Test
  def inputTypesPlusOne(): Unit = {
    val types = BenchmarkUserFun.createInputTypes(plusOne)

    val expected = Seq(ArrayType(Float, N), ArrayType(Int, N))
    assertEquals(expected, types)
  }

  @Test
  def inputTypesAdd(): Unit = {
    val types = BenchmarkUserFun.createInputTypes(add)

    val expected = Seq(ArrayType(Float, N), ArrayType(Float, N), ArrayType(Int, N))
    assertEquals(expected, types)
  }

  @Test
  def inputTypesAddPair(): Unit = {
    val types = BenchmarkUserFun.createInputTypes(addPair)

    val expected = Seq(ArrayType(TupleType(Float, Float), N), ArrayType(TupleType(Float, Float), N), ArrayType(Int, N))
    assertEquals(expected, types)
  }

  @Test
  def wrapper(): Unit = {
    BenchmarkUserFun(add, 1024)
  }

//  @Test
//  def inputTypesVec(): Unit = {
//    val types = BenchmarkUserFun.createInputTypes(VectorizeUserFun(4, add))
//
//    val expected = Seq(ArrayType(Float4, N), ArrayType(Float4, N), ArrayType(Int, N))
//    assertEquals(expected, types)
//  }

  @Test
  def bla2(): Unit = {

  }

  @Test
  def bla(): Unit = {

    val inputSize = 1024*1024
    val input = Array.tabulate(inputSize)(_ => util.Random.nextFloat())
    val testbla = Array.tabulate(inputSize)(_ => 3)

    val uf = plusOne
    val f = BenchmarkUserFun(uf, 2048)

    val (local, global) = InferNDRange(f)

    val code = Compile(f, local, global)

    val kernelCode = BenchmarkUserFun.spliceInUserFun(code, uf)

    val (_, time) = Execute().benchmark[Array[Float]](101, 200, kernelCode, f, input, testbla)

    println(time.sorted.apply(time.length/2))

  }

}
