package opencl.executor

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.generator.NDRange
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

/** Thrown when the executor receive ill-formed/ill-typed arguments */
class IllegalKernelArgument(msg: String) extends IllegalArgumentException(msg)

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
   * Create a map which maps variables (e.g., N) to values (e.g, "1024")
   */
  object createValueMap {
    /**
     * Creates the map given a lambda and its arguments
     */
    def apply(f: Lambda, values: Any*): immutable.Map[ArithExpr, Cst] = {
      // sanity check: there must be as many inputs as parameters in f
      if (f.params.length != values.length)
        throw new IllegalArgumentException(
          s"""| Wrong number of argument.
              | Expected: ${f.params.length}. Got: ${values.length}"""
          .stripMargin
        )
      
      // Traverse the the arguments and their types simultaneously to fetch
      // Any length information
      val (caps, sizes) = fetchAllConstraints(f.params.map(_.t), values)
    
      // Look for errors in the inferred sizes and build a map out of the list
      // of constraints
      val cleanedSizes = cleanSizeConstraints(sizes)
    
      // Compute a value for each capacity variable and checks that it is
      // consistent with the previously inferred sizes.
      val cleanedCaps = cleanCapacityConstraints(caps, cleanedSizes)
    
      // Finally, merge the two maps and cast the values into arithmetic
      // expressions.
      (cleanedSizes ++ cleanedCaps).mapValues(Cst(_))
    }
    
    // -------------------------------
    // Below: private helper functions
    // -------------------------------
  
    /**
     * A class representing a size/capacity constraint:
     * - A size constraint has to be interpreted the following way:
     *   `(size) variable == value`
     * - A capacity constraint has to be interpreted the following way:
     *   `(capacity) variable >= value`
     *
     * @param variable the variable as an arithmetic expression (should be a Var?)
     * @param value a possible value / lower bound for this variable
     */
    case class Constraint(variable: ArithExpr, value: Int)
  
    /**
     * Compute a list of capacity constraints and a list of size constraints
     * given a list of types and a list of values supposed to be matching them.
     */
    private def fetchAllConstraints(tys: Seq[Type], values: Seq[Any]): (Seq[Constraint], Seq[Constraint]) = {
      // Look for constraints in all the pairs like `(type, value)`
      // Hack: if vectors are passed to a kernel *not* wrapped in an array,
      //       they are passed as scalar types and they will be casted into
      //       vector later by OpenCL.
      //       For example:
      //       - a float4 should be passed as a Float
      //       - but an array of float4 must be passed as a 2D array of Floats
      val constraints =
        (tys zip values)
        .map(p => {
          val (ty, value) = p
          ty match {
            case VectorType(st, _) =>
              checkParamWithValue(st, value)
              (Seq.empty, Seq.empty)
            case _ =>
              val (caps, sizes) = fetchConstraints(ty, value)
              (simplify(caps), simplify(sizes))
          }
        })
      
      // Reshape this set of information into two sequences of constraints
      val (capacityConstraints2D, sizeConstraints2D) = constraints.unzip
      (capacityConstraints2D.flatten, sizeConstraints2D.flatten)
    }
  
    /**
     * Takes a list of size constraints, checks that they are consistent
     * altogether and build map from variable to values out of this list.
     */
    private def cleanSizeConstraints(sizeConstraints: Seq[Constraint]): immutable.Map[ArithExpr, Int] = {
      val map = sizeConstraints.groupBy(_.variable)
      
      // Check that the sizes are consistent (see issue #98, snippet 2)
      map.mapValues(pairs => {
        // If we have more than one value for a given key (N = 16 and N = 32),
        // throw an exception
        val values = pairs.map(_.value).distinct
        val variable = pairs.head.variable
        if (values.length != 1)
          throw new IllegalKernelArgument(
            s"Incompatible values inferred for variable: $variable\n" +
              values.map(v => s" - `$v`").mkString("\n")
          )
        // otherwise we keep that value
        values.head
      })
    }
  
    /**
     * Takes a list of capacity constraints and the map containing the inferred
     * sizes, infers a value for each capacity variable and checks that it is
     * consistent with the inferred sizes.
     */
    private def cleanCapacityConstraints(capacityConstraints: Seq[Constraint],
                                         sizes: immutable.Map[ArithExpr, Int]): immutable.Map[ArithExpr, Int] = {
      val map = capacityConstraints.groupBy(_.variable)
      
      // Take the maximum value for each variable.
      // e.g. if [1, 3] and [0, 0, 0] have both capacity N, then N must be at
      //      least 3 so we choose N = 3.
      map.mapValues(pairs => {
          val max = pairs.map(_.value).max
          val variable = pairs.head.variable
          // If the variable is also a size variable, check that this size
          // is at least the maximum capacity inferred. In this case the
          // occurrence of this variable in this Map will be ignored and we
          // keep the value found in the "sizes" Map.
          if (sizes.isDefinedAt(variable) && sizes(variable) < max)
            throw new IllegalKernelArgument(
              s"Overflow: ${sizes(variable)} has been inferred for size " +
              s"variable $variable but $variable is also the capacity of an " +
              s"array with $max elements."
            )
          max
        })
    }
  
    /**
     * Traverses a value and its Lift type and produces:
     * - a list of size constraints:       var == array length
     * - a list of capacity constraints:   var >= array length
     *
     * @param ty the type
     * @param value the value
     */
    private def fetchConstraints(ty: Type, value: Any): (Seq[Constraint], Seq[Constraint]) = {
      ty match {
        case at: ArrayType =>
          val array = asArray(value)
          val len = array.length / tupleSize(at.elemT)
        
          // Recursive call if array of arrays
          val (caps, sizes) = at.elemT match {
            case _: ArrayType =>
              val (cs, ss) = array.map(fetchConstraints(at.elemT, _)).toSeq.unzip
              (cs.flatten, ss.flatten)
            case _ =>
              checkParamWithValue(at.elemT, array.head)
              (Seq.empty, Seq.empty)
          }
        
          // fetch information for the current array
          (collectCapacityConstraints(at, len, caps), collectSizeConstraints(at, len, sizes))
        case TupleType(_) | ScalarType(_, _) =>
          // We assume tuples do not contain arrays
          checkParamWithValue(ty, value)
          (Seq.empty, Seq.empty)
        case VectorType(_, _) =>
          checkParamWithValue(ty, value)
          (Seq.empty, Seq.empty)
        case NoType | UndefType =>
          throw new IllegalArgumentException("Executor: Untyped parameter in lambda")
      }
    }
  
    /**
     * Type-checks a bit of kernel argument.
     * Arrays are handled in fetchConstraints using `asArray` and a recursive
     * call to itself since more work is required for them.
     */
    private def checkParamWithValue(t: Type, v: Any): Unit = {
      (t, v) match {
        case (Float,   _: Float) => // fine
        case (Int,   _: Int) => // fine
        case (Double,   _: Double) => // fine
        case (Bool, _: Boolean) => // fine
      
        case (VectorType(st, len), _) =>
          // Vectors must be passed as arrays
          if (!v.isInstanceOf[Array[_]])
            throw TypeException(
              s"Expected Array[$st] of size $len (representing a vector). " +
                s"Got ${v.getClass} instead."
            )
          val array = v.asInstanceOf[Array[_]]
          // Validate the underlying type and the length
          st match {
            case Float | Int | Double =>
            case _ => throw TypeException(s"$t is not a valid vector type")
          }
          val headType = Type.fromAny(array.head)
          if (headType != st || array.length != len.eval)
            throw TypeException(
              s"Expected Array[$st] of size $len (representing a vector). " +
                s"Got Array[$headType] of length ${array.length} instead."
            )
      
        // handle tuples if all their components are of the same type
        case (tt: TupleType, _: Float)
          if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Float) => // fine
        case (tt: TupleType, _: Int)
          if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Int) => // fine
        case (tt: TupleType, _: Double)
          if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Double) => // fine
        case (tt: TupleType, _: Boolean)
          if (tt.elemsT.distinct.length == 1) && (tt.elemsT.head == Bool) => // fine
      
        // Arrays are already handled by `fetchConstraints`
        case (_: ArrayType, _) =>
          throw new NotImplementedError("Sould not reach this point")
      
        case _ => throw new IllegalArgumentException(
          s"Expected value of type $t, but value of type ${v.getClass} given")
      }
    }

    /**
     * Tries to interpret a value as an array and throws a useful exception
     * if it cannot (not just a `ClassCastException`)
     */
    private def asArray(any: Any): Array[_] = any match {
      case array: Array[_] => array
      case _ => throw new IllegalKernelArgument(s"Array expected, got: ${any.getClass}")
    }
  
    /**
     * Tuples are given to the executor in a flattened format, we
     * have to take this into consideration while inferring the sizes.
     */
    private def tupleSize(ty: Type): Int = ty match {
      case tt: TupleType => tt.elemsT.length
      case _ => 1
    }
  
    /**
     * Infers a capacity constraint and appends it to the current sequence of
     * constraints.
     */
    private def collectCapacityConstraints(ty: ArrayType,
                         len: Int,
                         caps: Seq[Constraint]): Seq[Constraint] = {
      ty match {
        case c: Capacity => c.capacity match {
          case Cst(n) =>
            // Capacity must be at least the actual size
            if (n.toInt < len)
              throw new IllegalKernelArgument(s"Ill-sized argument: $n < $len")
            caps
          case expr => Constraint(expr, len) +: caps
        }
        case _ => caps
      }
    }
  
    /**
     * Infers a size constraint and appends it to the current sequence of
     * constraints.
     */
    private def collectSizeConstraints(ty: ArrayType, len: Int,
                          sizes: Seq[Constraint]): Seq[Constraint] = {
      ty match {
        case s: Size => s.size match {
          case Cst(n) =>
            // Look for ill-sized inputs. See issue #98, snippet 3
            if (n.toInt != len)
              throw new IllegalKernelArgument(s"Ill-sized argument: $n ≠ $len")
            sizes
          case expr => Constraint(expr, len) +: sizes
        }
        case _ => sizes
      }
    }
  
    /**
     * Simplify a list of constraints
     */
    private def simplify(c: Seq[Constraint]): Seq[Constraint] = c.map({
      case Constraint(v, len) =>
        val newLen = SolveForVariable(v, len).eval
        Constraint(v.varList.head, newLen)
    })
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
    var (localSizes, globalSizes) = InferNDRange(f)

    val defaultSize =
      globalSizes.numberOfDimensionsNotOne match {
        case 3 => 8
        case 2 => 16
        case 1 => 128
        case _ => 1
      }

    if (localSizes(0) == ?)
      localSizes = localSizes.copy(x = if (globalSizes(0) != Cst(1)) defaultSize else 1)

    if (localSizes(1) == ?)
      localSizes = localSizes.copy(y = if (globalSizes(1) != Cst(1)) defaultSize else 1)

    if (localSizes(2) == ?)
      localSizes = localSizes.copy(z = if (globalSizes(2) != Cst(1)) defaultSize else 1)

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
            s"Failed to infer evaluable local thread counts, ${realLocal.toString}")

        if (realGlobal.exists(!_.isEvaluable))
          throw new InvalidIndexSpaceException(
            s"Failed to infer evaluable global thread counts, ${realGlobal.toString}")

        (realLocal, realGlobal)

      } else {
        (
          NDRange(localSize1, localSize2, localSize3),
          NDRange(globalSize1, globalSize2, globalSize3)
        )
      }

    // sanity checks
    ValidateNDRange(globalSize(0).eval, localSize(0).eval, 0)
    ValidateNDRange(globalSize(1).eval, localSize(1).eval, 1)
    ValidateNDRange(globalSize(2).eval, localSize(2).eval, 2)
    ValidateGroupSize(localSize(0).eval * localSize(1).eval * localSize(2).eval)

    if (Verbose()) {
      println(s"Local sizes: ${localSize.toString}")
      println(s"Global sizes: ${globalSize.toString}")
    }

    (localSize, globalSize)
  }
  
  /**
   * Here we are computing an meaningful expression for the size of the inputs
   * when the `OpenCLMemoryAllocator` class has not been able to do it.
   * See the comments at the top of `OpenCLMemoryAllocator.scala` for further
   * information.
   *
   * @param ty the type of the value
   * @param value the value for which we are allocating memory
   * @return the size in bytes of this value once encoded
   */
  private def allocArgumentWithoutFixedAllocatedSize(ty: Type, value: Any): ArithExpr = {
    (ty, value) match {
      case (ScalarType(_, size), _) => size
      case (VectorType(st, len), _) => len.eval * st.size
      case (TupleType(elemsT @ _*), _) if elemsT.distinct.length == 1 =>
        elemsT.length * allocArgumentWithoutFixedAllocatedSize(elemsT.head, value)
      case (at: ArrayType, array: Array[_]) =>
        val c = at match {
          case c: Capacity => c.capacity
          case _ => Cst(array.length)
        }
        if (at.elemT.hasFixedAllocatedSize)
          at.getHeaderSize * 4 + c * 4 * allocArgumentWithoutFixedAllocatedSize(at.elemT, array.head)
        else
          at.getHeaderSize + c + array.map(allocArgumentWithoutFixedAllocatedSize(at.elemT, _)).reduce(_ + _)
      case _ => throw new IllegalArgumentException()
    }
  }

  private def execute[T](executeFunction: (Int, Int, Int, Int, Int, Int, Array[KernelArg]) => T,
                         f: Lambda, values: Any*): (Array[_], T) = {
    // 1. If some inputs could not been allocated so far because of some
    //    unknown capacity in their type, we have a last chance to do it now
    //    because we have their values.
    for ((p, value) <- f.params zip values) {
      if (p.mem.size == ?) {
        val size = allocArgumentWithoutFixedAllocatedSize(p.t, value)
        p.mem = OpenCLMemory(p.mem.variable, size, GlobalMemory)
      }
    }
    
    // 2. create map associating Variables, e.g., SizeVar("N"), with values, e.g., "1024".
    //    Also type-checks the inputs.
    val valueMap = Execute.createValueMap(f, values: _*)

    val (localSize, globalSize) = getAndValidateSizesForExecution(f, valueMap)

    // 3. make sure the device has enough memory to execute the kernel
    validateMemorySizes(f, valueMap)

    // 4. create output OpenCL kernel argument
    val outputSize = ArithExpr.substitute(Type.getMaxAllocatedSize(f.body.t), valueMap).eval
    val outputData = global(outputSize)

    // 5. create all OpenCL data kernel arguments
    val memArgs = createMemArgs(f, outputData, valueMap, values:_*)

    // 6. create OpenCL arguments reflecting the size information for the data arguments
    val sizes = createSizeArgs(f, valueMap)

    // 7. combine kernel arguments. first pointers and data, then the size information
    val args: Array[KernelArg] = memArgs ++ sizes

    // 8. execute via JNI and get the runtime (or runtimes)
    val t = this.synchronized {
      executeFunction(localSize(0).eval, localSize(1).eval, localSize(2).eval,
        globalSize(0).eval, globalSize(1).eval, globalSize(2).eval, args)
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

  private def createMemArgs(f: Lambda,
                            outputData: KernelArg,
                            valueMap: immutable.Map[ArithExpr, ArithExpr],
                            values: Any*): Array[KernelArg] = {
    // go through all memory objects associated with the generated kernel
    OpenCLGenerator.getMemories(f)._2.map(mem => {
      // get the OpenCL memory object ...
      val m = mem.mem
      val size = ArithExpr.substitute(m.size, valueMap).eval
      // ... look for it in the parameter list ...
      val i = f.params.indexWhere(m == _.mem)
      // ... if found create an OpenCL kernel argument from the matching runtime value ...
      if (i != -1) arg(values(i), Type.substitute(mem.t, valueMap), size)
      // ... if not found but it is the output set this ...
      else if (m == f.body.mem) outputData
      // ... else create a fresh local or global object argument
      else m.addressSpace match {
        case LocalMemory => local(size)
        case GlobalMemory => global(size)
        case s => throw new IllegalArgumentException(s"Invalid address space $s")
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
        Option(arg(s, Int, 4))
      }
      // ... else return nothing
      else Option.empty
    } ).filter(_.isDefined).map(_.get)
  }

  /**
   * Factory functions for creating OpenCL kernel arguments
   */
  object arg {
    /** Entry point for creating an OpenCL kernel argument */
    def apply(any: Any, ty: Type, size: Int): KernelArg = ty match {
      // Scalars and vectors that are not nested in an array
      case Bool  => value(any.asInstanceOf[Boolean])
      case Int | VectorType(Int, _)       => value(any.asInstanceOf[Int])
      case Float | VectorType(Float, _)   => value(any.asInstanceOf[Float])
      case Double | VectorType(Double, _) => value(any.asInstanceOf[Double])
      // Arrays
      case at: ArrayType =>
        val array = any.asInstanceOf[Array[_]]
        flatTupleBaseType(at) match {
          case Int =>
            val encoder = new Encoder[Int](n => n)
            val raw = encoder.encode(array, at, size/4)
            global.input(raw)
          case Float =>
            val encoder = new Encoder[Float](_.toFloat)
            val raw = encoder.encode(array, at, size/4)
            global.input(raw)
          case Double =>
            val encoder = new Encoder[Double](_.toDouble)
            val raw = encoder.encode(array, at, size/4)
            global.input(raw)
          case Bool =>
            val encoder = new Encoder[Boolean](_ != 0) // This is very bad
          val raw = encoder.encode(array, at, size)
            global.input(raw)
        }
      case _ => throw new EncodeError(ty)
    }
    
    /** Raise this exception if something cannot be encoded */
    private class EncodeError(ty: Type)
      extends IllegalArgumentException(s"Cannot encode type $ty")
  
    @scala.annotation.tailrec
    private def flatTupleBaseType(ty: Type): ScalarType = {
      Type.getBaseType(ty) match {
        case tt: TupleType if tt.elemsT.distinct.length == 1 =>
          flatTupleBaseType(tt.elemsT.head)
        case st: ScalarType => st
        case _ => throw new EncodeError(ty)
      }
    }
  
    /**
     * Factory for marshalling a scala value
     *
     * @param cast function used to convert the eventual headers and offsets
     *             of an array into the type of the elements of the array.
     *             TODO: think about this limitation, the headers should remain integers
     * @tparam T the underlying scalar type of the array we are encoding
     *           (e.g. for a 3D array of floats, this is `Float`)
     */
    private class Encoder[T: ClassTag](cast: Int => T) {
      /** Public wrapper for the encode method below */
      def encode(array: Array[_], at: ArrayType, size: Int): Array[T] = {
        val buffer = Array.fill(size)(cast(0))
        val nextPos = encode(buffer, 0, array, at)
        assert(nextPos <= size) // Sanity check
        buffer
      }
  
      /**
       * Turn a scala array of arbitrary dimension into a flat representation
       * with all the necessary headers and offsets and write it to `buffer`
       *
       * @param buffer where to write the encoded array
       * @param pos at what position in `buffer` to write the encoded array
       * @param array the array to encode
       * @param at the Lift type of the array
       * @return the next position in buffer we should write to
       */
      private def encode(buffer: Array[T], pos: Int,
                         array: Array[_], at: ArrayType): Int = {
        val newPos = writeHeader(buffer, pos, array, at)
        at.elemT match {
          case _: ScalarType | _: TupleType =>
            // Copy
            array.asInstanceOf[Array[T]].copyToArray(buffer, newPos)
            // Return the next position
            getNextPos(at, newPos, newPos + array.length)
          case _: VectorType =>
            val flat = array.asInstanceOf[Array[Array[T]]].flatten
            flat.copyToArray(buffer ,newPos)
            // Compute the next position
            getNextPos(at, newPos, newPos + flat.length)
          case elemT: ArrayType =>
            val v = array.asInstanceOf[Array[Array[_]]]
            if (elemT.hasFixedAllocatedSize) {
              // Copy
              val endPos = v.foldLeft(newPos)(encode(buffer, _, _, elemT))
              // Compute the next position
              getNextPos(at, newPos, endPos)
            } else {
              // Copy
              var ofsIdx = newPos
              val capacity = at match {
                case c: Capacity => c.capacity.eval
                case _ => array.length
              }
              v.foldLeft(newPos + capacity)((p, arr) => {
                val pAfter = encode(buffer, p, arr, elemT)
                buffer(ofsIdx) = cast(p - newPos) // store the offset
                ofsIdx = ofsIdx + 1
                pAfter
              }) // May the force be with you…
            }
          case ty => throw new EncodeError(ty)
        }
      }
      
      // -------------------------------
      // Below: private helper functions
      // -------------------------------
  
      /**
       * The size a type will take once encoded in a Scala array.
       */
      private def sizeOf(ty: Type): ArithExpr = {
        val allocated = Type.getAllocatedSize(ty)
        val nbBytes = Type.getAllocatedSize(Type.fromAny(cast(0)))
        allocated / nbBytes
      }
  
      /**
       * Writes the header of this array into `buffer` at position `pos`. The
       * shape of the header is specified in issue #107 and depends on the type
       * `at` of the array while the content of the header depends on the
       * actual length of `array`.
       * TODO: move this specification to `docs/` (and update it)
       *
       * @return the next position where we should write in the buffer
       *         (namely `pos` + header size)
       */
      private def writeHeader(buffer: Array[T], pos: Int,
                            array: Array[_], at: ArrayType): Int = {
        val hSize = at.getHeaderSize
        val len = cast(array.length)
        for (i <- pos until pos + hSize) buffer(i) = len
        pos + hSize
      }
  
      // TODO: I'm not very sweet
      private def getNextPos(at: ArrayType, before: Int, fallback: Int): Int = {
        at match {
          case c: Capacity =>
            // No evaluation exception should occur here, if something is not
            // evaluable, it should have been handled before.
            val nextPos = (before + c.capacity * sizeOf(at.elemT)).eval
            assert(fallback <= nextPos) // sanity check
            nextPos
          case _ => fallback
        }
      }
    }
  }

  /**
   * Create global argument allocated with the given size in bytes
   */
  object global {
    def apply(sizeInBytes: Long): GlobalArg = GlobalArg.createOutput(sizeInBytes)

    /**
     * Create global input arguments from an array
     */
    object input {
      def apply(array: Array[Float]): GlobalArg   = GlobalArg.createInput(array)
      def apply(array: Array[Int]): GlobalArg     = GlobalArg.createInput(array)
      def apply(array: Array[Double]): GlobalArg  = GlobalArg.createInput(array)
      def apply(array: Array[Boolean]): GlobalArg = GlobalArg.createInput(array)
    }

    /**
     * Create output argument given a Type and the number of elements
     */
    object output {
      def apply[T: ClassTag](length: Int): GlobalArg = {
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
    def apply(sizeInBytes: Long): LocalArg = LocalArg.create(sizeInBytes)
  }

  /**
   * Create a kernel argument passed by value
   */
  object value {
    def apply(value: Float): ValueArg   = ValueArg.create(value)
    def apply(value: Int): ValueArg     = ValueArg.create(value)
    def apply(value: Double): ValueArg  = ValueArg.create(value)
    def apply(value: Boolean): ValueArg = ValueArg.create(value)
  }
}
