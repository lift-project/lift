package opencl.executor

import apart.arithmetic.{ArithExpr, Cst, Var}
import arithmetic.TypeVar
import ir._
import ir.ast._
import opencl.generator.{OpenCLGenerator, Verbose}
import opencl.ir._

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
  def apply(globalSize: Int): Execute = {
    apply(128, globalSize)
  }

  /**
   *
   * These three functions create Execute instances with the given one/two/three dimensional local
   * and global sizes. The last parameter determines if the local and global size are injected in
   * the OpenCL kernel code.
   */
  def apply(localSize: Int,
            globalSize: Int,
            injectSizes: (Boolean, Boolean) = (false, false)): Execute = {
    // sanity checks
    ValidateNDRange(globalSize, localSize, 0)
    new Execute(localSize, 1, 1, globalSize, 1, 1, injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, globalSize1: Int,  globalSize2: Int,
            injectSizes: (Boolean, Boolean)): Execute = {
    // sanity checks
    ValidateNDRange(globalSize1, localSize1, 0)
    ValidateNDRange(globalSize2, localSize2, 1)
    new Execute(localSize1, localSize2, 1, globalSize1, globalSize2, 1,
                injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, localSize3: Int,
            globalSize1: Int,  globalSize2: Int, globalSize3: Int,
            injectSizes: (Boolean, Boolean)): Execute = {
    // sanity checks
    ValidateNDRange(globalSize1, localSize1, 0)
    ValidateNDRange(globalSize2, localSize2, 1)
    ValidateNDRange(globalSize3, localSize3, 2)
    new Execute(localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3,
                injectSizes._1, injectSizes._2)
  }

  /**
   * Private helper functions.
   * Create a map which maps variables (e.g., N) to values (e.g, "1024")
   */
  def createValueMap(f: Lambda, values: Any*): immutable.Map[ArithExpr, ArithExpr] = {
    // just take the variables
    val vars = f.params.flatMap((p) => Type.getLengths(p.t).filter(_.isInstanceOf[Var]))

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
      case any: Any
        => Seq(Cst(1))
    }).flatten[ArithExpr]

    (vars zip sizes).toMap[ArithExpr, ArithExpr]
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
      throw new InvalidLocalSizeException(s"Local size ($localSize) cannot be negative in dim $dim")
    if (globalSize <= 0)
      throw new InvalidGlobalSizeException(s"Global size ($globalSize) cannot be negative in dim $dim")
    if (globalSize % localSize != 0)
      throw new InvalidIndexSpaceException(s"Global size ($globalSize) is not divisible by local size ($localSize) in dim $dim")
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
class Execute(val localSize1: Int, val localSize2: Int, val localSize3: Int,
              val globalSize1: Int, val globalSize2: Int, val globalSize3: Int,
              val injectLocalSize: Boolean, val injectGroupSize: Boolean = false) {

  /**
   * Given just a string: evaluate the string into a lambda and then call the function below
   */
  def apply(input: String, values: Any*): (Any, Double) = {
    apply(Eval(input), values: _*)
  }

  /**
   * Given a lambda: compile it and then execute it
   */
  def apply(f: Lambda, values: Any*): (Any, Double) = {
    val code = compile(f, values:_*)

    execute(code, f, values: _*)
  }

  /**
   * Given a lambda: compile it and then execute it <code>iterations</code> times
   */
  def apply(iterations: Int, timeout: Double, f: Lambda, values: Any*): (Any, Double) = {
    val code = compile(f, values:_*)

    benchmark(iterations, timeout, code, f, values:_*)
  }

  private def compile(f: Lambda, values: Any*) : String = {
    // 1. choice: local and work group size should be injected into the OpenCL kernel ...
    if (injectLocalSize && injectGroupSize) {
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

    val executeFunction: (String, Int, Int, Int, Int, Int, Int, Array[KernelArg]) => Double =
      (code, localSize1, localSize2, localSize3,
       globalSize1, globalSize2, globalSize3, args) =>
        Executor.execute(code, localSize1, localSize2, localSize3,
          globalSize1, globalSize2, globalSize3, args)

    execute(executeFunction, code, f, values:_*)
  }

  /**
   * Execute given source code, which was compiled for the given lambda, with the given runtime
   * values <code>iterations</code> times. If the kernel takes longer than <code>timeout</code> ms,
   * it is executed only once.
   * Returns a pair consisting of the computed values as its first and the median runtime as its second
   * component
   */
  def benchmark(iterations: Int, timeout: Double, code: String, f: Lambda, values: Any*): (Array[_], Double) = {

    val executeFunction: (String, Int, Int, Int, Int, Int, Int, Array[KernelArg]) => Double =
      (code, localSize1, localSize2, localSize3,
         globalSize1, globalSize2, globalSize3, args) =>
      Executor.benchmark(code, localSize1, localSize2, localSize3,
        globalSize1, globalSize2, globalSize3, args, iterations, timeout)

    execute(executeFunction, code, f, values:_*)
  }

  private def execute(executeFunction: (String, Int, Int, Int, Int, Int, Int, Array[KernelArg]) => Double,
                       code: String, f: Lambda, values: Any*): (Array[_], Double) = {

    // check that the given values match with the given lambda expression
    checkParamsWithValues(f.params, values)

    // 1. create map associating Variables, e.g., Var("N"), with values, e.g., "1024".
    val valueMap: immutable.Map[ArithExpr, ArithExpr] = Execute.createValueMap(f, values: _*)

    // 2. check all group functions (introduced for stencil support) are valid arguments for the
    // given input sizes
    staticGroupCheck(f, valueMap)

    // 3. make sure the device has enough memory to execute the kernel
    validateMemorySizes(valueMap)

    // 4. create output OpenCL kernel argument
    val outputSize = ArithExpr.substitute(Type.getSize(f.body.t), valueMap).eval
    val outputData = global(outputSize)

    // 5. create all OpenCL data kernel arguments
    val memArgs = createMemArgs(f, outputData, valueMap, values:_*)

    // 6. create OpenCL arguments reflecting the size information for the data arguments
    val sizes = createSizeArgs(f, valueMap)

    // 7. combine kernel arguments. first pointers and data, then the size information
    val args: Array[KernelArg] = memArgs ++ sizes

    // 8. execute via JNI
    val runtime = executeFunction(code, localSize1, localSize2, localSize3,
      globalSize1, globalSize2, globalSize3, args)

    // 9. cast the output accordingly to the output type
    val output = castToOutputType(f.body.t, outputData)

    // 10. release OpenCL objects
    args.foreach(_.dispose)

    // 11. return output data and runtime as a tuple
    (output, runtime)
  }

  /** Check that all possible indices returned by Group calls are in-bounds */
  private def staticGroupCheck(f: Lambda, valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    val groupFuns = Expr.visitWithState(Set[Group]())(f.body, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case group: Group => set + group
          case _ => set
        }
        case _ => set
      })

    for (g <- groupFuns) {
      val allIndices = g.relIndices.min to g.relIndices.max

      g.params(0).t match {
        case ArrayType(_, lenExpr) =>
          val length = ArithExpr.substitute(lenExpr, valueMap).eval

          for (relIdx <- allIndices) {
            var newIdx = 0
            if (relIdx < 0) {
              newIdx = g.negOutOfBoundsF(relIdx, length).eval
            } else if (relIdx > 0) {
              newIdx = g.posOutOfBoundsF(relIdx, length).eval
            }

            if (newIdx < 0 || newIdx >= length) {
              throw new IllegalArgumentException("Group function would map relative out-of-bounds" +
                                                 " index " + relIdx + " to new illegal index " +
                                                 newIdx + ".")
            }
          }
        case _ => throw new IllegalArgumentException("Expect parameter to be of ArrayType")
      }
    }
  }

  private def castToOutputType(t: Type, outputData: GlobalArg): Array[_] = {
    assert(t.isInstanceOf[ArrayType])
    Type.getBaseType(t) match {
      case Float => outputData.asFloatArray()
      case Int   => outputData.asIntArray()
      // handle tuples if all their components are of the same type
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Float) =>
        outputData.asFloatArray()
      case t: TupleType if (t.elemsT.distinct.length == 1) && (t.elemsT.head == Int) =>
        outputData.asIntArray()
      case _ => throw new IllegalArgumentException("Return type of the given lambda expression " +
                                                   "not supported: " + t.toString)
    }
  }

  private def checkParamsWithValues(params: Seq[Param], values : Seq[Any]): Unit = {
    if (params.length != values.length) {
      throw new IllegalArgumentException("Expected " + params.length + " parameters, but " +
                                         values.length + " given")
    }

    (params, values).zipped.foreach( (p, v) => checkParamWithValue(p.t, v) )
  }

  private def checkParamWithValue(t: Type, v: Any): Unit = {
    (t, v) match {
      case (at: ArrayType, av: Array[_]) => checkParamWithValue(at.elemT, av(0))
      case (Float,   _: Float) => // fine
      case (Float2,  _: Float) => //fine
      case (Float3,  _: Float) => //fine
      case (Float4,  _: Float) => //fine
      case (Float8,  _: Float) => //fine
      case (Float16, _: Float) => //fine

      case (Int,   _: Int) => // fine
      case (Int2,  _: Int) => //fine
      case (Int3,  _: Int) => //fine
      case (Int4,  _: Int) => //fine
      case (Int8,  _: Int) => //fine
      case (Int16, _: Int) => //fine

      // handle tuples if all their components are of the same type
      case (tt: TupleType, _: Float)
        if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Float) => // fine
      case (tt: TupleType, _: Int)
        if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Int) => // fine
      case _ => throw new IllegalArgumentException("Expected value of type " + t.toString + ", " +
                                                   "but " + "value of type " + v.getClass
                                                                               .toString + " given")
    }
  }

  private def createMemArgs(f: Lambda,
                            outputData: KernelArg,
                            valueMap: immutable.Map[ArithExpr, ArithExpr],
                            values: Any*): Array[KernelArg] = {
    // go through all memory objects associated with the generated kernel
    OpenCLGenerator.Kernel.memory.map(mem => {
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

  private def validateMemorySizes(valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    val (globalMemories, localMemories) =
      (OpenCLGenerator.Kernel.memory ++ OpenCLGenerator.Kernel.staticLocalMemory).
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
    val allVars = OpenCLGenerator.Kernel.memory.map(mem => {
      mem.mem.size.varList
    } ).filter(_.nonEmpty).flatten.distinct
    // select the variables which are not (internal) iteration variables
    val (vars, _) = allVars.partition(_.name != Iterate.varName)

    // go through all size variables associated with the kernel
    vars.map( v => {
      // look for the variable in the parameter list ...
      val i = f.params.indexWhere( p => p.t.varList.contains(v) )
      // ... if found look up the runtime value in the valueMap and create kernel argument ...
      if (i != -1) {
        val s = valueMap(v).eval
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

        case _ => throw new IllegalArgumentException("Kernel argument is of unsupported type: " +
                                                     any.getClass.toString)
      }
    }
  }

  /**
   * Create global argument allocated with the given size in bytes
   */
  object global {
    def apply(sizeInBytes: Int) = GlobalArg.createOutput(sizeInBytes)

    /**
     * Create global input arguments from an array
     */
    object input {
      def apply(array: Array[Float]) = GlobalArg.createInput(array)

      def apply(array: Array[Int]) = GlobalArg.createInput(array)
    }

    /**
     * Create output argument given a Type and the number of elements
     */
    object output {
      def apply[T: ClassTag](length: Int) = {
        implicitly[ClassTag[T]] match {
          case ClassTag.Float => GlobalArg.createOutput(length * 4) // in bytes
          case ClassTag.Int => GlobalArg.createOutput(length * 4) // in bytes
          case tag => throw new IllegalArgumentException("Given type: " + tag.toString() + " not " +
                                                       "supported")
        }
      }
    }
  }

  /**
   * Create local argument allocated with the given size in bytes
   */
  object local {
    def apply(sizeInBytes: Int) = LocalArg.create(sizeInBytes)
  }

  /**
   * Create a kernel argument passed by value
   */
  object value {
    def apply(value: Float) = ValueArg.create(value)

    def apply(value: Int) = ValueArg.create(value)
  }

}
