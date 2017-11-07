package analysis

import ir._
import ir.ast._
import lift.arithmetic.{Cst, SizeVar}
import opencl.executor._
import opencl.generator._
import opencl.ir._
import opencl.ir.pattern._
import prog_gen.InputGenerator
import rewriting.InferNDRange

object BenchmarkUserFun {

  val N = SizeVar("N")

  def benchmark(uf: UserFun, callsPerThread: Int, iterations: Int, timeout: Double = 200): Double = {
    val inputSize = 1024*1024

    val testbla = Array.tabulate(inputSize)(_ => 3)

    val f = BenchmarkUserFun(uf, callsPerThread)

    val generator = new InputGenerator(collection.Map(N -> Cst(inputSize)))
    val inputs = f.params.init.map(p => generator(p.t)) :+ testbla

    val (local, global) = InferNDRange(f)

    val code = Compile(f, local, global)
    val kernelCode = BenchmarkUserFun.spliceInUserFun(code, uf)

    val (_, time) = Execute().benchmark[Array[Float]](iterations, timeout, kernelCode, f, inputs:_*)
    val median = time.sorted.apply(time.length/2)

    median
  }

  def spliceInUserFun(code: String, uf: UserFun): String = {
    val lines = code.split("\n")

    val kernelStartLine = lines.indexWhere(_.contains("benchmark" + uf.name))

    val userFunDefinition = OpenCLGenerator.createFunctionDefinition(uf)

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
