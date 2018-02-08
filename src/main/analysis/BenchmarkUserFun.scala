package analysis

import generic.ast.AstPrinter
import ir._
import ir.ast._
import lift.arithmetic.{Cst, SizeVar}
import opencl.executor._
import opencl.generator._
import opencl.ir._
import opencl.ir.pattern._
import prog_gen.InputGenerator
import rewriting.InferNDRange
import utils.Printer

object BenchmarkUserFun {

  val N = SizeVar("N")

  val conditionValue = 3

  def benchmark(uf: UserFun, callsPerThread: Int, iterations: Int, timeout: Double = 200): Double = {

    val f = createBenchmarkingLambda(uf, callsPerThread)

    val inputs = createInputs(f)

    val (local, global) = InferNDRange(f)

    val originalCode = Compile(f, local, global)
    val kernelCode = spliceInBenchmarkedFunction(originalCode, uf)

    val (_, time) = Execute().benchmark[Array[Float]](iterations, timeout, kernelCode, f, inputs: _*)
    val median = time.sorted.apply(time.length / 2)

    median
  }

  def createInputs(f: Lambda): Array[Any] = {
    val inputSize = 1024 * 1024

    val conditionInput = Array.tabulate(inputSize)(_ => conditionValue)

    val generator = new InputGenerator(collection.Map(N -> Cst(inputSize)))
    val userFunInputs = f.params.init.map(p => generator(p.t))

    userFunInputs :+ conditionInput
  }

  /**
   * Return the kernel code with the definition for `uf`.
   */
  def spliceInBenchmarkedFunction(code: String, uf: UserFun): String = {
    val lines = code.split("\n")

    val benchmarkingFunctionLine = lines.indexWhere(_.contains("benchmark" + uf.name))

    val userFunDefinition = OpenCLGeneratorOld.createFunctionDefinition(uf)
    val printedUserFun = AstPrinter(userFunDefinition)()

    val beginLines = lines.take(benchmarkingFunctionLine - 1).mkString("\n")
    val endLines = lines.drop(benchmarkingFunctionLine - 1).mkString("\n")

    val kernelCode = beginLines + s"\n\n$printedUserFun\n\n" + endLines

    kernelCode
  }

  def createBenchmarkingLambda(uf: UserFun, callsPerThread: Int): Lambda = {

    val inputTypes = createInputTypes(uf)

    val params = inputTypes.map(Param.apply)

    val benchmarkingFun = createBenchmarkingFunction(uf, callsPerThread)

    Lambda(params.toArray, MapGlb(benchmarkingFun) $ Zip(params: _*))
  }

  def createInputTypes(uf: UserFun): Seq[Type] =
    uf.inTs.map(ArrayType(_, N)) :+ ArrayType(Int, N)

  def createBenchmarkingFunction(uf: UserFun, count: Int): UserFun = {
    val wrapperArgumentNames = uf.paramNames :+ "conditionValue"
    val inputTypes = uf.inTs :+ Int

    val outputTypeName = Printer.toString(uf.outT)

    val increment =
      (uf.inTs, uf.paramNames).zipped.map((t, name) => createIncrement(t, name)).mkString(" ")

    val body =
      s"""{
         |  int count = $count;
         |  $outputTypeName my_output;
         |
         |  for (int i = 0; i < count; i++) {
         |    if (conditionValue <= $conditionValue + i) {
         |      my_output = ${uf.name}(${uf.paramNames.mkString(", ")});
         |    } else {
         |      $increment
         |    }
         |  }
         |
         |  return my_output;
         |}
      """.stripMargin

    UserFun("benchmark" + uf.name, wrapperArgumentNames, body, inputTypes, uf.outT)
  }

  /**
   * Create the OpenCL C code to increment all fields in the variable `name` with type `t`.
   */
  def createIncrement(t: Type, name: String): String = t match {
    case Float | Double | Int | Long => s"$name = $name + 1;"
    case VectorType(basicType, n)    =>
      (0 until n.eval).map(i => createIncrement(basicType, s"$name.s$i")).mkString(" ")
    case TupleType(tt@_*)            =>
      tt.zipWithIndex.map(pair => createIncrement(pair._1, s"$name._${pair._2}")).mkString(" ")
    case illegalType                 => throw new IllegalArgumentException(illegalType.toString)
  }

}
