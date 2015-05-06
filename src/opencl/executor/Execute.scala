package opencl.executor

import arithmetic.{Var, Cst, ArithExpr}
import ir._
import opencl.generator.{Verbose, OpenCLGenerator}
import opencl.ir._

import scala.collection.immutable
import scala.reflect.ClassTag

/** Thrown when the global size is not divisible by the local size */
class InvalidIndexSpaceException(msg: String) extends Exception(msg)
/** Thrown on negative or 0 global size */
class InvalidGlobalSizeException(msg: String) extends Exception(msg)
/** Thrown on negative or 0 local size */
class InvalidLocalSizeException(msg: String) extends Exception(msg)

/**
 * Interface for executing a lambda object in OpenCL via Java -> JNI -> SkelCL -> OpenCL
 */

/**
 * This object provides factory functions for creating an instance of the class Execute
 */
object Execute {
  /**
   * Creates an Execute instance with the given one dimensional global size and a default local size.
   * Neither the global nor the local size is injected in the OpenCL kernel code.
   */
  def apply(globalSize: Int): Execute = {
    apply(128, globalSize)
  }

  /**
   *
   * These three functions create Execute instances with the given one/two/three dimensional local and global sizes.
   * The last parameter determines if the local and global size are injected in the OpenCL kernel code.
   */
  def apply(localSize: Int, globalSize: Int, injectSizes: (Boolean, Boolean) = (false, false)): Execute = {
    // sanity checks
    ValidateNDRange(globalSize, localSize, 0)
    new Execute(localSize, 1, 1, globalSize, 1, 1, injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, globalSize1: Int,  globalSize2: Int,
            injectSizes: (Boolean, Boolean)): Execute = {
    // sanity checks
    ValidateNDRange(globalSize1, localSize1, 0)
    ValidateNDRange(globalSize2, localSize2, 1)
    new Execute(localSize1, localSize2, 1, globalSize1, globalSize2, 1, injectSizes._1, injectSizes._2)
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
   */
  def createValueMap(f: Lambda, values: Any*): immutable.Map[ArithExpr, ArithExpr] = {
    // just take the variables
    val vars = f.params.map((p) => Type.getLengths(p.t).filter(_.isInstanceOf[Var])).flatten

    val tupleSizes = f.params.map(_.t match {
      case ArrayType(ArrayType(ArrayType(tt: TupleType, _), _), _) => tt.elemsT.length
      case ArrayType(ArrayType(tt: TupleType, _), _) => tt.elemsT.length
      case ArrayType(tt: TupleType, _) => tt.elemsT.length
      case tt: TupleType => tt.elemsT.length
      case ArrayType(ArrayType(ArrayType(vt: VectorType, _), _), _) => vt.len.eval()
      case ArrayType(ArrayType(vt: VectorType, _), _) => vt.len.eval()
      case ArrayType(vt: VectorType, _) => vt.len.eval()
      case vt: VectorType => vt.len.eval()
      case _ => 1
    })

    val sizes = (values, tupleSizes).zipped.map((value, tupleSize) => value match {
      case aaaa: Array[Array[Array[Array[_]]]]
        => Seq(Cst(aaaa.length), Cst(aaaa(0).length), Cst(aaaa(0)(0).length), Cst(aaaa(0)(0)(0).length / tupleSize))
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
    if(localSize <= 0)
      throw new InvalidLocalSizeException("Local size (" + localSize + ") cannot be negative in dim " + dim)
    if(globalSize <= 0)
      throw new InvalidGlobalSizeException("Global size (" + localSize + ") cannot be negative in dim " + dim)
    if(globalSize % localSize != 0)
      throw new InvalidIndexSpaceException("Global size (" + globalSize +
                                           ") is not divisible by local size (" + localSize + ") in dim " + dim)
  }
}

/**
 * For executing a lambda an instance of this class is created (e.g., by using one of the above factory functions).
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
  def apply(input: String, values: Any*): (Array[Float], Double) = {
    apply(Eval(input), values:_*)
  }

  /**
   * Given a lambda: compile it and the execute it
   */
  def apply(f: Lambda, values: Any*): (Array[Float], Double) = {

    // 1. choice: local and work group size should be injected into the OpenCL kernel ...
    if (injectLocalSize && injectGroupSize) {
      // ... build map of values mapping size information to arithmetic expressions, e.g., ???
      val valueMap = Execute.createValueMap(f, values:_*)
      // ... compile with all information provided
      val code = Compile(f, localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3, valueMap)
      // .. finally execute
      return execute(code, f, values:_*)
    }

    // 2.choice: local size should be injected into the OpenCL kernel ...
    if (injectLocalSize) {
      // ... compile with providing local size information
      val code = Compile(f, localSize1, localSize2, localSize3)
      // .. finally execute
      return execute(code, f, values:_*)
    }

    // 3.choice: nothing should we injected into the OpenCL kernel ... just compile and execute
    execute(Compile(f), f, values:_*)
  }

  /**
   * Given a compiled code as a string and the corresponding lambda execute it.
   *
   * This function can be used for debug purposes, where the OpenCL kernel code is changed slightly
   * but the corresponding lambda can remain unchanged.
   */
  def apply(code: String, f: Lambda, values: Any*) : (Array[Float], Double) = {
    execute(code, f, values:_*)
  }

  /**
   * Execute given source code, which was compiled for the given lambda, with the given runtime values.
   * Returns a pair consisting of the computed values as its first and the runtime as its second component
   */
  def execute(code: String, f: Lambda, values: Any*) : (Array[Float], Double) = {

    val valueMap: immutable.Map[ArithExpr, ArithExpr] = Execute.createValueMap(f, values:_*)

    val outputSize = ArithExpr.substitute(Type.getSize(f.body.t), valueMap).eval()

    // Check all Group functions valid arguments for the given input sizes
    staticGroupCheck(f, valueMap)

    val inputs = values.map({
      case f: Float => value(f)
      case af: Array[Float] => global.input(af)
      case aaf: Array[Array[Float]] => global.input(aaf.flatten)
      case aaaf: Array[Array[Array[Float]]] => global.input(aaaf.flatten.flatten)
      case aaaf: Array[Array[Array[Array[Float]]]] => global.input(aaaf.flatten.flatten.flatten)


      case i: Int => value(i)
      case ai: Array[Int] => global.input(ai)
      case aai: Array[Array[Int]] => global.input(aai.flatten)
    })
    val outputData = global(outputSize)

    val memArgs = OpenCLGenerator.Kernel.memory.map( mem => {
      val m = mem.mem
      val i = f.params.indexWhere( m == _.mem )
      if (i != -1) inputs(i)
      else if (m == f.body.mem) outputData
      else m.addressSpace match {
        case LocalMemory => local(ArithExpr.substitute(m.size, valueMap).eval())
        case GlobalMemory => global(ArithExpr.substitute(m.size, valueMap).eval())
      }
    })

    val args: Array[KernelArg] = (memArgs ++ inputs).distinct

    if (Verbose())
      println("args.length " + args.length)

    val runtime = Executor.execute(code, localSize1, localSize2, localSize3,
      globalSize1, globalSize2, globalSize3, args)

    val output = outputData.asFloatArray()

    args.foreach(_.dispose)

    (output, runtime)
  }

  /** Check that all possible indices returned by Group calls are in-bounds */
  def staticGroupCheck(f: Lambda, valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    val groupFuns = Expr.visit(Set[Group]())(f.body, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case group: Group => set + group
          case _ => set
        }
        case _ => set
      })

    for (g <- groupFuns) {
      val allIndices = g.relIndices.min to g.relIndices.max

      g.params(0).t match  {
        case ArrayType(_, lenExpr) =>
          val length = ArithExpr.substitute(lenExpr, valueMap).eval()

          for (relIdx <- allIndices) {
            var newIdx = 0
            if (relIdx < 0) {
              newIdx = g.negOutOfBoundsF(relIdx, length).eval()
            } else if (relIdx > 0) {
              newIdx = g.posOutOfBoundsF(relIdx, length).eval()
            }

            if (newIdx < 0 || newIdx >= length) {
              throw new IllegalArgumentException("Group function would map relative out-of-bounds index " + relIdx +
                " to new illegal index " + newIdx + ".")
            }
          }
      }
    }
  }

}

/**
 * Factory functions for creating OpenCL kernel arguments
 */

object global {
  /**
   * Create global input arguments from an array
   */
  object input {
    def apply(array: Array[Float]) = GlobalArg.createInput(array)
    def apply(array: Array[Int])   = GlobalArg.createInput(array)
  }

  /**
   * Create output argument given a Type and the number of elements
   */
  object output {
    def apply[T : ClassTag](length: Int) = {
      implicitly[ClassTag[T]] match {
        case ClassTag.Float => GlobalArg.createOutput(length * 4) // in bytes
        case ClassTag.Int   => GlobalArg.createOutput(length * 4) // in bytes
        case _ => throw new IllegalArgumentException
      }
    }
  }

  /**
   * Create global argument allocated with the given size in bytes
   */
  def apply(sizeInBytes: Int) = GlobalArg.createOutput(sizeInBytes)
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
  def apply(value: Int)   = ValueArg.create(value)
}
