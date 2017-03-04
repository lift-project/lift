package opencl.executor

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator.{OpenCLGenerator, Verbose}
import opencl.ir._
import rewriting.InferNDRange

import scala.collection.immutable
import scala.reflect.ClassTag

/** Thrown when the global size is not divisible by the local size */
class InvalidIndexSpaceException(msg: String) extends Exception(msg)
/** Thrown on negative or 0 global size */
class InvalidGlobalSizeException(msg: String) extends Exception(msg)
/** Thrown on negative or 0 local size */
class InvalidLocalSizeException(msg: String) extends Exception(msg)

/** Thrown when the device cannot execute the kernel */
class DeviceCapabilityException(msg: String) extends RuntimeException(msg)

/**
 * Interface for executing a lambda object in OpenCL via Java -> JNI -> SkelCL -> OpenCL
 */

/**
 * This object provides factory functions for creating an instance of the class Execute
 */
object Execute {
  /**
   * Creates an Execute instance with the given one dimensional global size and a default local
   * size. Neither the global nor the local size is injected in the OpenCL kernel code.
   */
  def apply(globalSize: Int): Execute =
    apply(128, globalSize)

  /**
   *
   * These three functions create Execute instances with the given one/two/three dimensional local
   * and global sizes. The last parameter determines if the local and global size are injected in
   * the OpenCL kernel code.
   */
  def apply(localSize: Int,
            globalSize: Int,
            injectSizes: (Boolean, Boolean) = (false, false)): Execute = {
    new Execute(localSize, 1, 1, globalSize, 1, 1, injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, globalSize1: Int,  globalSize2: Int,
            injectSizes: (Boolean, Boolean)): Execute = {
    new Execute(localSize1, localSize2, 1, globalSize1, globalSize2, 1,
                injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, localSize3: Int,
            globalSize1: Int,  globalSize2: Int, globalSize3: Int,
            injectSizes: (Boolean, Boolean)): Execute = {

    new Execute(localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3,
                injectSizes._1, injectSizes._2)
  }


  def apply(localSize: NDRange,
            globalSize: NDRange,
            injectSizes: (Boolean, Boolean)): Execute = {

    new Execute(localSize(0), localSize(1), localSize(2),
                globalSize(0), globalSize(1), globalSize(2),
                injectSizes._1, injectSizes._2)
  }

  /**
    * Creates an Execute instance that tries to automatically infer local and global
    * sizes to minimise generated loops and injects them into the kernel.
    *
    * @see [[rewriting.InferNDRange]] for more details on the inference process.
    *
    */
  def apply(): Execute =
    new Execute (?, ?, ?, ?, ?, ?, true, true)

  /**
   * Private helper functions.
   * Create a map which maps variables (e.g., N) to values (e.g, "1024")
   */
  def createValueMap(f: Lambda, values: Any*): immutable.Map[ArithExpr, ArithExpr] = {
    val sizeExprs = f.params.flatMap((p) => Type.getLengths(p.t).init)

    val tupleSizes = f.params.map(_.t match {
      case ArrayType(ArrayType(ArrayType(tt: TupleType, _), _), _) => tt.elemsT.length
      case ArrayType(ArrayType(tt: TupleType, _), _) => tt.elemsT.length
      case ArrayType(tt: TupleType, _) => tt.elemsT.length
      case tt: TupleType => tt.elemsT.length
      case ArrayType(ArrayType(ArrayType(vt: VectorType, _), _), _) => vt.len.eval
      case ArrayType(ArrayType(vt: VectorType, _), _) => vt.len.eval
      case ArrayType(vt: VectorType, _) => vt.len.eval
      case vt: VectorType => vt.len.eval
      case _ => 1
    })

    val sizes = (values, tupleSizes).zipped.map((value, tupleSize) => value match {
      case aaaa: Array[Array[Array[Array[_]]]]
        => Seq(Cst(aaaa.length), Cst(aaaa(0).length),
               Cst(aaaa(0)(0).length), Cst(aaaa(0)(0)(0).length / tupleSize))
      case aaa: Array[Array[Array[_]]]
        => Seq(Cst(aaa.length), Cst(aaa(0).length), Cst(aaa(0)(0).length / tupleSize))
      case aa: Array[Array[_]]
        => Seq(Cst(aa.length), Cst(aa(0).length / tupleSize))
      case a: Array[_]
        => Seq(Cst(a.length / tupleSize))
      case _
        => Seq(Cst(1))
    }).flatten[ArithExpr]

    (sizeExprs zip sizes).flatMap{
      case (Cst(_), _) => Seq() // ignore
      case pair => Seq((pair._1.varList.head, SolveForVariable(pair._1, pair._2)))
    }.toMap[ArithExpr, ArithExpr]
  }

  /**
   * Helper function to run sanity checks on the global and local size.
   * @param globalSize Global range
   * @param localSize Local range
   * @param dim Current dimension
   * @throws InvalidLocalSizeException if localSize == 0
   *         InvalidGlobalSizeException if GlobalSize == 0
   *         InvalidIndexSpaceException if GlobalSize % localSize != 0
   */
  private def ValidateNDRange(globalSize: Int, localSize: Int, dim: Int): Unit = {
    if (localSize <= 0)
      throw new InvalidLocalSizeException(
        s"Local size ($localSize) cannot be negative in dim $dim")
    if (globalSize <= 0)
      throw new InvalidGlobalSizeException(
        s"Global size ($globalSize) cannot be negative in dim $dim")
    if (globalSize % localSize != 0)
      throw new InvalidIndexSpaceException(
        s"Global size ($globalSize) is not divisible by local size ($localSize) in dim $dim")
  }

  private def ValidateGroupSize(localSize: Int): Unit = {
    val maxWorkGroupSize = Executor.getDeviceMaxWorkGroupSize

    if (localSize > maxWorkGroupSize)
      throw new DeviceCapabilityException(
        s"Device ${Executor.getDeviceName} can't execute kernels with " +
        s"work-groups larger than $maxWorkGroupSize.")
  }
}

/**
 * For executing a lambda an instance of this class is created (e.g., by using one of the above
 * factory functions).
 * @param localSize1      local size in dim 0
 * @param localSize2      local size in dim 1
 * @param localSize3      local size in dim 2
 * @param globalSize1     global size in dim 0
 * @param globalSize2     global size in dim 1
 * @param globalSize3     global size in dim 2
 * @param injectLocalSize should the OpenCL local size be injected into the kernel code?
 * @param injectGroupSize should the size of an OpenCL work group be injected into the kernel code?
 */
class Execute(val localSize1: ArithExpr, val localSize2: ArithExpr, val localSize3: ArithExpr,
              val globalSize1: ArithExpr, val globalSize2: ArithExpr, val globalSize3: ArithExpr,
              val injectLocalSize: Boolean, val injectGroupSize: Boolean = false) {

  import Execute._

  /**
   * Given just a string: evaluate the string into a lambda and
   * then call the function below
   */
  def apply(input: String, values: Any*): (Any, Double) = {
    apply(Eval(input), values: _*)
  }

  /**
   * Given a lambda: compile it and then execute it
   */
  def apply(f: Lambda, values: Any*): (Any, Double) = {
    val kernel = compile(f, values:_*)

    execute(kernel, f, values: _*)
  }

  /**
   * Given a lambda: compile it and then execute it <code>iterations</code> times
   */
  def apply(iterations: Int, timeout: Double, f: Lambda, values: Any*): (Any, Array[Double]) = {
    val kernel = compile(f, values:_*)

    benchmark(iterations, timeout, kernel, f, values:_*)
  }

  def evaluate(iterations: Int, timeout: Double, f: Lambda, values: Any*): (Any, Double) = {
    val kernel = compile(f, values:_*)

    evaluate(iterations, timeout, kernel, f, values:_*)
  }

  private def compile(f: Lambda, values: Any*) : String = {
    // 1. choice: local and work group size should be injected into the OpenCL kernel ...
    if (injectLocalSize && injectGroupSize) {

      if (shouldInferSizes) {

        val (localSizes: NDRange, globalSizes: NDRange) = inferSizes(f)

        return Compile(f, localSizes(0), localSizes(1), localSizes(2),
          globalSizes(0), globalSizes(1), globalSizes(2), immutable.Map())

      }

      // ... build map of values mapping size information to arithmetic expressions, e.g., ???
      val valueMap = Execute.createValueMap(f, values: _*)
      // ... compile with all information provided
      return Compile(f, localSize1, localSize2, localSize3,
        globalSize1, globalSize2, globalSize3, valueMap)
    }

    // 2.choice: local size should be injected into the OpenCL kernel ...
    if (injectLocalSize) {
      // ... compile with providing local size information
      return Compile(f, localSize1, localSize2, localSize3)
    }

    // 3.choice: nothing should we injected into the OpenCL kernel ... just compile
    Compile(f)
  }

  // Unknown local and global sizes indicate they should be inferred
  private def shouldInferSizes =
    localSize1 == ? && localSize2 == ? && localSize3 == ? &&
      globalSize1 == ? && globalSize2 == ? && globalSize3 == ?

  private def inferSizes(f: Lambda) = {
    val (localSizes, globalSizes) = InferNDRange(f)

    val existingDims = globalSizes.count(_ != Cst(1))

    val defaultSize =
      existingDims match {
        case 3 => 8
        case 2 => 16
        case 1 => 128
        case _ => 1
      }

    if (localSizes(0) == ?)
      localSizes(0) = if (globalSizes(0) != Cst(1)) defaultSize else 1

    if (localSizes(1) == ?)
      localSizes(1) = if (globalSizes(1) != Cst(1)) defaultSize else 1

    if (localSizes(2) == ?)
      localSizes(2) = if (globalSizes(2) != Cst(1)) defaultSize else 1

    (localSizes, globalSizes)
  }

  /**
   * Given a compiled code as a string and the corresponding lambda execute it.
   *
   * This function can be used for debug purposes, where the OpenCL kernel code is changed slightly
   * but the corresponding lambda can remain unchanged.
   */
  def apply(code: String, f: Lambda, values: Any*): (Any, Double) = {
    execute(code, f, values: _*)
  }

  /**
   * Execute given source code, which was compiled for the given lambda, with the given runtime
   * values.
   * Returns a pair consisting of the computed values as its first and the runtime as its second
   * component
   */
  def execute(code: String, f: Lambda, values: Any*): (Array[_], Double) = {

    val executeFunction: (Int, Int, Int, Int, Int, Int, Array[KernelArg]) => Double =
      (localSize1, localSize2, localSize3,
       globalSize1, globalSize2, globalSize3, args) => {
        val kernel = Build(code)
        try {
          Executor.execute(kernel, localSize1, localSize2, localSize3,
            globalSize1, globalSize2, globalSize3, args)
        } finally {
          kernel.dispose()
        }
      }

    execute(executeFunction, f, values:_*)
  }

  /**
   * Execute given source code, which was compiled for the given lambda, with the given runtime
   * values <code>iterations</code> times. If the kernel takes longer than <code>timeout</code> ms,
   * it is executed only once. If <code>timeout</code> is <code>0.0</code> no check for the kernel
   * runtime will be performed.
   *
   * Returns a pair consisting of the computed values as its first and an array of runtimes in the
    * order of execution as its second component
   */
  def benchmark(iterations: Int, timeout: Double, code: String, f: Lambda, values: Any*): (Array[_], Array[Double]) = {

    val executeFunction: (Int, Int, Int, Int, Int, Int, Array[KernelArg]) => Array[Double] =
      (localSize1, localSize2, localSize3,
         globalSize1, globalSize2, globalSize3, args) => {
        val kernel = Build(code)
        try {
          Executor.benchmark(kernel, localSize1, localSize2, localSize3,
            globalSize1, globalSize2, globalSize3, args, iterations, timeout)
        } finally {
          kernel.dispose()
        }
      }

    execute(executeFunction, f, values:_*)
  }

  def evaluate(iterations: Int, timeout: Double, code: String, f: Lambda, values: Any*): (Array[_], Double) = {

    val executeFunction: (Int, Int, Int, Int, Int, Int, Array[KernelArg]) => Double =
      (localSize1, localSize2, localSize3,
         globalSize1, globalSize2, globalSize3, args) => {
        val kernel = Build(code)
        try {
          Executor.execute(kernel, localSize1, localSize2, localSize3,
            globalSize1, globalSize2, globalSize3, args)
        } finally {
          kernel.dispose()
        }
      }

    execute(executeFunction, f, values:_*)
  }

  private[executor] def getAndValidateSizesForExecution(f: Lambda,
    valueMap: immutable.Map[ArithExpr,ArithExpr]): (NDRange, NDRange) = {

    val (localSize, globalSize) =
      if (shouldInferSizes) {

        val (local, global) = inferSizes(f)
        val realLocal = InferNDRange.substituteInNDRange(local, valueMap)
        val realGlobal = InferNDRange.substituteInNDRange(global, valueMap)

        if (realLocal.exists(!_.isEvaluable))
          throw new InvalidIndexSpaceException(
            s"Failed to infer evaluable local thread counts, ${realLocal.mkString(", ")}")

        if (realGlobal.exists(!_.isEvaluable))
          throw new InvalidIndexSpaceException(
            s"Failed to infer evaluable global thread counts, ${realGlobal.mkString(", ")}")

        (realLocal, realGlobal)

      } else {
        (
          Array[ArithExpr](localSize1, localSize2, localSize3),
          Array[ArithExpr](globalSize1, globalSize2, globalSize3)
        )
      }

    // sanity checks
    ValidateNDRange(globalSize(0).evalToInt, localSize(0).evalToInt, 0)
    ValidateNDRange(globalSize(1).evalToInt, localSize(1).evalToInt, 1)
    ValidateNDRange(globalSize(2).evalToInt, localSize(2).evalToInt, 2)
    ValidateGroupSize(localSize(0).evalToInt * localSize(1).evalToInt * localSize(2).evalToInt)

    if (Verbose()) {
      println(s"Local sizes: ${localSize.mkString(", ")}")
      println(s"Global sizes: ${globalSize.mkString(", ")}")
    }

    (localSize, globalSize)
  }

  private def execute[T](executeFunction: (Int, Int, Int, Int, Int, Int, Array[KernelArg]) => T,
                         f: Lambda, values: Any*): (Array[_], T) = {

    // 1. check that the given values match with the given lambda expression
    checkParamsWithValues(f.params, values)

    // 2. create map associating Variables, e.g., SizeVar("N"), with values, e.g., "1024".
    val valueMap = Execute.createValueMap(f, values: _*)

    val (localSize, globalSize) = getAndValidateSizesForExecution(f, valueMap)

    // 3. make sure the device has enough memory to execute the kernel
    validateMemorySizes(f, valueMap)

    // 4. create output OpenCL kernel argument
    val outputSize = ArithExpr.substitute(Type.getMaxSize(f.body.t), valueMap).evalToInt
    val outputData = global(outputSize)

    // 5. create all OpenCL data kernel arguments
    val memArgs = createMemArgs(f, outputData, valueMap, values:_*)

    // 6. create OpenCL arguments reflecting the size information for the data arguments
    val sizes = createSizeArgs(f, valueMap)

    // 7. combine kernel arguments. first pointers and data, then the size information
    val args: Array[KernelArg] = memArgs ++ sizes

    // 8. execute via JNI and get the runtime (or runtimes)
    val t = this.synchronized {
      executeFunction(localSize(0).evalToInt, localSize(1).evalToInt, localSize(2).evalToInt,
        globalSize(0).evalToInt, globalSize(1).evalToInt, globalSize(2).evalToInt, args)
    }

    // 9. cast the output accordingly to the output type
    val output = castToOutputType(f.body.t, outputData)

    // 10. release OpenCL objects
    args.foreach(_.dispose)

    // 11. return output data and runtime as a tuple
    (output, t)
  }

  private def castToOutputType(t: Type, outputData: GlobalArg): Array[_] = {
    assert(t.isInstanceOf[ArrayType])
    Type.getBaseType(t) match {
      case Float => outputData.asFloatArray()
      case Int   => outputData.asIntArray()
      case Double   => outputData.asDoubleArray()
      case Bool  => outputData.asBooleanArray()
      // handle tuples if all their components are of the same type
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float2) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float3) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float4) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float8) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float16) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int) =>
        outputData.asIntArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int2) =>
        outputData.asIntArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int3) =>
        outputData.asIntArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int4) =>
        outputData.asIntArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int8) =>
        outputData.asIntArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int16) =>
        outputData.asIntArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Double) =>
        outputData.asDoubleArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Double2) =>
        outputData.asDoubleArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Double3) =>
        outputData.asDoubleArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Double4) =>
        outputData.asDoubleArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Double8) =>
        outputData.asDoubleArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Double16) =>
        outputData.asDoubleArray()
      case _ => throw new IllegalArgumentException(
        s"Return type of the given lambda expression not supported: $t")
    }
  }

  private def checkParamsWithValues(params: Seq[Param], values : Seq[Any]): Unit = {
    if (params.length != values.length)
      throw new IllegalArgumentException(
        s"Expected ${params.length} parameters, but ${values.length} given")

    (params, values).zipped.foreach( (p, v) => checkParamWithValue(p.t, v) )
  }

  @scala.annotation.tailrec
  private def checkParamWithValue(t: Type, v: Any): Unit = {
    (t, v) match {
      case (at: ArrayType, av: Array[_]) => checkParamWithValue(at.elemT, av(0))
      case (Float,   _: Float) => // fine
      case (Int,   _: Int) => // fine
      case (Double,   _: Double) => // fine
      case (Bool, _: Boolean) => // fine

      case (VectorType(Float, _), _) => //fine
      case (VectorType(Int, _), _) => //fine
      case (VectorType(Double, _), _) => //fine

      // handle tuples if all their components are of the same type
      case (tt: TupleType, _: Float)
        if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Float) => // fine
      case (tt: TupleType, _: Int)
        if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Int) => // fine
      case (tt: TupleType, _: Double)
        if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Double) => // fine
      case (tt: TupleType, _: Boolean)
        if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Bool) => // fine
      case _ => throw new IllegalArgumentException(
        s"Expected value of type $t, but value of type ${v.getClass} given")
    }
  }

  private def createMemArgs(f: Lambda,
                            outputData: KernelArg,
                            valueMap: immutable.Map[ArithExpr, ArithExpr],
                            values: Any*): Array[KernelArg] = {
    // go through all memory objects associated with the generated kernel
    OpenCLGenerator.getMemories(f)._2.map(mem => {
      // get the OpenCL memory object ...
      val m = mem.mem
      // ... look for it in the parameter list ...
      val i = f.params.indexWhere(m == _.mem)
      // ... if found create an OpenCL kernel argument from the matching runtime value ...
      if (i != -1) arg(values(i))
      // ... if not found but it is the output set this ...
      else if (m == f.body.mem) outputData
      // ... else create a fresh local or global object argument
      else m.addressSpace match {
        case LocalMemory => local(ArithExpr.substitute(m.size, valueMap).eval)
        case GlobalMemory => global(ArithExpr.substitute(m.size, valueMap).eval)
      }
    })
  }

  private def validateMemorySizes(f:Lambda, valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    val memories = OpenCLGenerator.getMemories(f)

    val (globalMemories, localMemories) =
      (memories._1 ++ memories._2).
        partition(_.mem.addressSpace == GlobalMemory)

    val globalSizes = globalMemories.map(mem => ArithExpr.substitute(mem.mem.size, valueMap).eval)
    val totalSizeOfGlobal = globalSizes.sum
    val totalSizeOfLocal = localMemories.map(mem =>
      ArithExpr.substitute(mem.mem.size, valueMap).eval).sum

    globalSizes.foreach(size => {
      val maxMemAllocSize = Executor.getDeviceMaxMemAllocSize
      if (size > maxMemAllocSize)
        throw new DeviceCapabilityException(s"Buffer size required ($size) cannot be larger than $maxMemAllocSize")
    })

    val globalMemSize = Executor.getDeviceGlobalMemSize
    if (totalSizeOfGlobal > globalMemSize)
      throw new DeviceCapabilityException(s"Global size required ($totalSizeOfGlobal) cannot be larger than $globalMemSize")

    val localMemSize = Executor.getDeviceLocalMemSize
    if (totalSizeOfLocal > localMemSize)
      throw new DeviceCapabilityException(s"Local size required ($totalSizeOfLocal) cannot be larger than $localMemSize")
  }

  private def createSizeArgs(f: Lambda,
    valueMap: immutable.Map[ArithExpr, ArithExpr]): Array[KernelArg] = {
    // get the variables from the memory objects associated with the generated kernel
    val allVars = OpenCLGenerator.getMemories(f)._2.map(
      _.mem.size.varList
     ).filter(_.nonEmpty).flatten.distinct
    // select the variables which are not (internal) iteration variables
    val (vars, _) = allVars.partition(_.name != Iterate.varName)

    // go through all size variables associated with the kernel
    vars.sortBy(_.name).map( v => {
      // look for the variable in the parameter list ...
      val i = f.params.indexWhere( p => p.t.varList.contains(v) )
      // ... if found look up the runtime value in the valueMap and create kernel argument ...
      if (i != -1) {
        val s = valueMap(v).eval
        //noinspection SideEffectsInMonadicTransformation
        if (Verbose())
          println(s)
        Option(arg(s))
      }
      // ... else return nothing
      else Option.empty
    } ).filter(_.isDefined).map(_.get)
  }

  /**
   * Factory functions for creating OpenCL kernel arguments
   */

  object arg {
    def apply(any: Any) = {
      any match {
        case f: Float => value(f)
        case af: Array[Float] => global.input(af)
        case aaf: Array[Array[Float]] => global.input(aaf.flatten)
        case aaaf: Array[Array[Array[Float]]] => global.input(aaaf.flatten.flatten)
        case aaaaf: Array[Array[Array[Array[Float]]]] => global.input(aaaaf.flatten.flatten.flatten)

        case i: Int => value(i)
        case ai: Array[Int] => global.input(ai)
        case aai: Array[Array[Int]] => global.input(aai.flatten)
        case aaai: Array[Array[Array[Int]]] => global.input(aaai.flatten.flatten)
        case aaaai: Array[Array[Array[Array[Int]]]] => global.input(aaaai.flatten.flatten.flatten)

        case d: Double => value(d)
        case ad: Array[Double] => global.input(ad)
        case aad: Array[Array[Double]] => global.input(aad.flatten)
        case aaad: Array[Array[Array[Double]]] => global.input(aaad.flatten.flatten)
        case aaaad: Array[Array[Array[Array[Double]]]] => global.input(aaaad.flatten.flatten.flatten)

        case d: Boolean => value(d)
        case ad: Array[Boolean] => global.input(ad)
        case aad: Array[Array[Boolean]] => global.input(aad.flatten)
        case aaad: Array[Array[Array[Boolean]]] => global.input(aaad.flatten.flatten)
        case aaaad: Array[Array[Array[Array[Boolean]]]] => global.input(aaaad.flatten.flatten.flatten)

        case _ => throw new IllegalArgumentException(
          s"Kernel argument is of unsupported type: ${any.getClass}")
      }
    }
  }

  /**
   * Create global argument allocated with the given size in bytes
   */
  object global {
    def apply(sizeInBytes: Long) = GlobalArg.createOutput(sizeInBytes)

    /**
     * Create global input arguments from an array
     */
    object input {
      def apply(array: Array[Float]) = GlobalArg.createInput(array)

      def apply(array: Array[Int]) = GlobalArg.createInput(array)

      def apply(array: Array[Double]) = GlobalArg.createInput(array)

      def apply(array: Array[Boolean]) = GlobalArg.createInput(array)
    }

    /**
     * Create output argument given a Type and the number of elements
     */
    object output {
      def apply[T: ClassTag](length: Int) = {
        implicitly[ClassTag[T]] match {
          case ClassTag.Float => GlobalArg.createOutput(length * 4) // in bytes
          case ClassTag.Int => GlobalArg.createOutput(length * 4) // in bytes
          case tag =>
            throw new IllegalArgumentException(s"Given type: $tag not supported")
        }
      }
    }
  }

  /**
   * Create local argument allocated with the given size in bytes
   */
  object local {
    def apply(sizeInBytes: Long) = LocalArg.create(sizeInBytes)
  }

  /**
   * Create a kernel argument passed by value
   */
  object value {
    def apply(value: Float) = ValueArg.create(value)

    def apply(value: Int) = ValueArg.create(value)

    def apply(value: Double) = ValueArg.create(value)

    def apply(value: Boolean) = ValueArg.create(value)
  }

}
