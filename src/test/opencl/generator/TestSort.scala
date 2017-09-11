package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.{Predicate, _}
import opencl.executor._
import opencl.generator.OpenCLAST.ArithExpression
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.language.postfixOps
import reflect.ClassTag

object TestSort {
  @BeforeClass def TestSort(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}


class TestSort {

  /*
    perform a single layer of bitonic shuffling - this is our gold standard for a layer of the sorting operation
   */
  def bitonic_iteration[T <% Ordered[T]](arr: Seq[T], dim_i: Int, dim_j: Int)(implicit ev1: ClassTag[T]): Seq[T] = {
    // iterate over the arr, and calculate the new value...
    val new_arr: Seq[T] = arr.zipWithIndex.map { case (elem, idx) => compare_and_replace(arr, idx, dim_i, dim_j) }
    new_arr
  }

  /*
  A single compare-and-replace in a bitonic sort
   */
  def compare_and_replace[T <% Ordered[T]](arr: Seq[T], index: Int, dim_i: Int, dim_j: Int)(implicit ev1: ClassTag[T]): T = {
    // get our value:
    val index_val: T = arr(index)
    //    println(s"index + val: ${index}, ${index_val}")
    // calculate our offset value
    val pair_index: Int = compute_pair(index, dim_j)
    // load the other value from that offset
    val pair_val: T = arr(pair_index)
    //    println(s"pair index + val: ${pair_index}, ${pair_val}")
    // direction is some function of dim_i - or rather the chunk is some function of dim_i...
    val direction = compute_direction(index, dim_i)
    val should_swap = direction == ((pair_val > index_val) == (pair_index > index))
    //    println(s"direction: ${direction}, comparison: ${pair_val > index_val}, index comparison: ${pair_index > index}, should_swap: ${should_swap}")

    val retval = if (should_swap) pair_val else index_val
    //    println(s"final value: ${retval}")
    retval
  }

  def compute_pair(index: Int, dim_j: Int): Int = {
    index ^ (1 << dim_j)
  }

  def compute_direction(index: Int, dim_i: Int): Boolean = {
    (index & (2 << dim_i)) != 0
  }

  def shuffle[T](a: Array[T]) = {
    for (i <- 1 until a.size) {
      val j = util.Random nextInt (i + 1)
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    a
  }


  @Test def print_bitonic_directions(): Unit = {
    val dimensions = 4
    val array_size = 1 << dimensions
    val arr = Array.tabulate(array_size)((i: Int) => i)
    var c = 0
    for (i <- 0 to dimensions - 1) {
      for (j <- 0 to i reverse) {
        println(s"${i}–${j}-${c}: " + arr.map { idx: Int => if (compute_direction(idx, i)) 1 else 0 }.mkString("[", ",", "]"))
        c = c + 1
      }
    }
  }

  @Test def print_bitonic_pairs(): Unit = {
    val dimensions = 4
    val array_size = 1 << dimensions
    val arr = Array.tabulate(array_size)((i: Int) => i)
    for (i <- 0 to dimensions - 1) {
      for (j <- 0 to i reverse) {
        println(arr.map { idx: Int => compute_pair(idx, j) }.mkString("[", ",", "]"))
      }
    }
  }

  @Test def bitonic_iteration(): Unit = {
    val dimensions = 4
    val array_size = 1 << dimensions

    var arr = Array.tabulate(array_size)((i: Int) => i).toSeq

    val arr2 = bitonic_iteration(arr, 0, 0)
    println(arr2.mkString("[", ",", "]"))
    arr = arr2
  }

  @Test def bitonic_sort_gold(): Unit = {
    val dimensions = 4
    val array_size = 1 << dimensions

    var arr = shuffle(Array.tabulate(array_size)((i: Int) => i).toArray).toSeq
    val gold = arr.sorted.toArray
    println(s"Array before: ${arr.mkString("[", ",", "]")}")
    for (i <- 0 to dimensions - 1) {
      for (j <- 0 to i reverse) {
        val arr2 = bitonic_iteration(arr, i, j)
        println(arr2.mkString("[", ",", "]"))
        arr = arr2
      }
    }
    assertArrayEquals(gold ,arr.toArray)
  }

  @Test def single_iteration_compile_time_indices(): Unit = {
    val inputSize = Math.pow(2, 8).toInt
    val arr = Array.tabulate(inputSize)((i: Int) => i.toFloat)
    val splitSize = 2

    val N = SizeVar("N")

    val idxArr = ArrayFromGenerator((i, N) => ArithExpression(i), ArrayTypeWSWC(Int, N))
    val tfiadd = UserFun("tuplefiadd", "x", "{return x._0 + (float)x._1;}", TupleType(Float, Int), Float);

    val hypercube_pair = (dim_j: ArithExpr) => (idx: ArithExpr, t: Type) => {
      (idx ^ (1 << dim_j))
    }

    val sort_direction = (dim_i: ArithExpr, idx: ArithExpr) => {
      Predicate(idx & (2 << dim_i), 0, Predicate.Operator.!=)
    }

    val direction_generator = (dim_i: ArithExpr) => ArrayFromGenerator((idx, _) => sort_direction(dim_i, idx), ArrayTypeWSWC(Int, N))

    val index_generator = ArrayFromGenerator((idx, _) => ArithExpression(idx), ArrayTypeWSWC(Int, N))

    val other_index_generator = (dim_j: ArithExpr) => ArrayFromGenerator((idx, _) => ArithExpression(hypercube_pair(dim_j)(idx, Int)), ArrayTypeWSWC(Int, N))

    val dim_i = 0
    val dim_j = 0

    val gold: Array[Float] = bitonic_iteration(arr.toSeq, dim_i, dim_j).toArray

    val select_new_value = UserFun("select_new_value", "x",
      """
        | // Tuple type Structure:
        | // x._0 = elem
        | // x._1 = other elem
        | // x._2 = index
        | // x._3 = other index
        | // x._4 = sort direction
        | int should_swap = x._4 == ((x._1 > x._0) == (x._3 > x._2));
        | return (should_swap ? x._1 : x._0);
      """.stripMargin, TupleType(Float, Float, Int, Int, Int), Float);


    // try it with dimension 0
    val kernel = fun(
      ArrayTypeWSWC(Float, N),
      (array) => {
        MapWrg(
          MapLcl(select_new_value)

        ) o Split(N) $ Zip(array, Gather(hypercube_pair(dim_j)) $ array, index_generator, other_index_generator(dim_j), direction_generator(dim_i))
      }
    )

    val (output: Array[Float], runtime) = Execute(1, 1)(kernel, arr)
    println("Time: " + runtime)
    println(s"Input:  ${arr.take(20).mkString("[", ",", "]")}")
    println(s"Output: ${output.take(20).mkString("[", ",", "]")}")
    assertArrayEquals(output, gold, 0.01f)
  }

  @Test def single_iteration_runtime_indices(): Unit = {
    val dimensions = 8
    val inputSize = Math.pow(2, dimensions).toInt
    var arr = shuffle(Array.tabulate(inputSize)((i: Int) => i.toFloat))

    val splitSize = 2

    val N = SizeVar("N")

    val index_generator = ArrayFromGenerator((idx, _) => ArithExpression(idx), ArrayTypeWSWC(Int, N))

    val dim_i = 0
    val dim_j = 0

    val gold: Array[Float] = bitonic_iteration(arr.toSeq, dim_i, dim_j).toArray

    val select_new_value = UserFun("select_new_value", Array("val", "o_val", "idx", "o_idx", "dir"),
      """
        | int should_swap = dir == ((o_val > val) == (o_idx > idx));
        | return (should_swap ? o_val : val);
      """.stripMargin, Array(Float, Float, Int, Int, Int), Float);

    val get_other_index = UserFun("get_other_index", Array("dim_j", "idx"),
      """
        | return (idx ^ (1  << dim_j));
      """.stripMargin, Array(Int, Int), Int)

    val sort_direction = UserFun("sort_direction", Array("dim_i", "idx"),
      """
        | return ((idx & (2  << dim_i)) != 0);
      """.stripMargin, Array(Int, Int), Int)

    val kernel = fun(
      ArrayTypeWSWC(Float, N),
      Int, Int,
      (array, dim_i, dim_j) => {
        MapWrg(
          MapLcl(fun(elemIxPair =>
            toGlobal(id) o toPrivate(Let(idx =>
              Let(dir =>
                Let(o_idx =>
                  Let(o_val =>
                    select_new_value(Get(elemIxPair, 0), o_val, idx, o_idx, dir)
                  ) o CheckedArrayAccess(o_idx, -1.0f) $ array
                ) $ get_other_index(dim_j, idx)
              ) $ sort_direction(dim_i, idx)
            )) $ Get(elemIxPair, 1)
          )
          )
        ) o Split(N) $ Zip(array, index_generator)
      }
    )

    val (output: Array[Float], runtime) = Execute(1, 1)(kernel, arr, 0, 0)


    println("Time: " + runtime)
    println(s"Array: ${arr.take(20).mkString("[",",","]")}")
    println(s"Output: ${output.take(20).mkString("[", ",", "]")}")
    println(s"Gold: ${gold.take(20).mkString("[", ",", "]")}")
    assertArrayEquals(output, gold, 0.01f)
  }

  @Test def full_sort_compile_time_indices(): Unit = {
    val dimensions = 8
    val inputSize = Math.pow(2, dimensions).toInt
    var arr = shuffle(Array.tabulate(inputSize)((i: Int) => i.toFloat))

    val splitSize = 2

    val N = SizeVar("N")

    val hypercube_pair = (dim_j: ArithExpr) => (idx: ArithExpr, t: Type) => {
      (idx ^ (1 << dim_j))
    }

    val sort_direction = (dim_i: ArithExpr, idx: ArithExpr) => {
      Predicate(idx & (2 << dim_i), 0, Predicate.Operator.!=)
    }

    val direction_generator = (dim_i: ArithExpr) => ArrayFromGenerator((idx, _) => sort_direction(dim_i, idx), ArrayTypeWSWC(Int, N))

    val index_generator = ArrayFromGenerator((idx, _) => ArithExpression(idx), ArrayTypeWSWC(Int, N))

    val other_index_generator = (dim_j: ArithExpr) => ArrayFromGenerator((idx, _) => ArithExpression(hypercube_pair(dim_j)(idx, Int)), ArrayTypeWSWC(Int, N))

    val dim_i = 0
    val dim_j = 0

    val gold: Array[Float] = arr.sorted

    val select_new_value = UserFun("select_new_value", "x",
      """
        | // Tuple type Structure:
        | // x._0 = elem
        | // x._1 = other elem
        | // x._2 = index
        | // x._3 = other index
        | // x._4 = sort direction
        | int should_swap = x._4 == ((x._1 > x._0) == (x._3 > x._2));
        | return (should_swap ? x._1 : x._0);
      """.stripMargin, TupleType(Float, Float, Int, Int, Int), Float);


    // try it with dimension 0
    var total_runtime = 0.0
    for (dim_i <- 0 to dimensions - 1) {
      for (dim_j <- 0 to dim_i reverse) {
        val kernel = fun(
          ArrayTypeWSWC(Float, N),
          (array) => {
            MapWrg(
              MapLcl(select_new_value)
            ) o Split(N) $ Zip(array, Gather(hypercube_pair(dim_j)) $ array, index_generator, other_index_generator(dim_j), direction_generator(dim_i))
          }
        )

        val (output: Array[Float], runtime) = Execute(1, 1)(kernel, arr)
        total_runtime = total_runtime + runtime
        arr = output
      }
    }

    println("Time: " + total_runtime)
    println(s"Output: ${arr.take(20).mkString("[", ",", "]")}")
    assertArrayEquals(arr, gold, 0.01f)
  }

  @Test def full_sort_runtime_indices(): Unit = {
    val dimensions = 8
    val inputSize = Math.pow(2, dimensions).toInt
    var arr = shuffle(Array.tabulate(inputSize)((i: Int) => i.toFloat))

    val splitSize = 128

    val N = SizeVar("N")

    val index_generator = ArrayFromGenerator((idx, _) => ArithExpression(idx), ArrayTypeWSWC(Int, N))

    val dim_i = 0
    val dim_j = 0

    val gold: Array[Float] = arr.sorted

    val select_new_value = UserFun("select_new_value", Array("val", "o_val", "idx", "o_idx", "dir"),
      """
        | int should_swap = dir == ((o_val > val) == (o_idx > idx));
        | return (should_swap ? o_val : val);
      """.stripMargin, Array(Float, Float, Int, Int, Int), Float);

    val get_other_index = UserFun("get_other_index", Array("dim_j", "idx"),
      """
        | return (idx ^ (1  << dim_j));
      """.stripMargin, Array(Int, Int), Int)

    val sort_direction = UserFun("sort_direction", Array("dim_i", "idx"),
      """
        | return ((idx & (2  << dim_i)) != 0);
      """.stripMargin, Array(Int, Int), Int)

    val kernel = fun(
      ArrayTypeWSWC(Float, N),
      Int, Int,
      (array, dim_i, dim_j) => {
        MapWrg(
          MapLcl(fun(elemIxPair =>
            toGlobal(id) o toPrivate(Let(idx =>
              Let(dir =>
                Let(o_idx =>
                  Let(o_val =>
                    select_new_value(Get(elemIxPair, 0), o_val, idx, o_idx, dir)
                  ) o CheckedArrayAccess(o_idx, -1.0f) $ array
                ) $ get_other_index(dim_j, idx)
              ) $ sort_direction(dim_i, idx)
            )) $ Get(elemIxPair, 1)
          )
          )
        ) o Split(N) $ Zip(array, index_generator)
      }
    )

    val code = Compile(kernel)

    var total_runtime = 0.0
    var iterations = 0
    for (dim_i <- 0 to dimensions - 1) {
      for (dim_j <- 0 to dim_i reverse) {
        val (output: Array[Float], runtime) = Execute(128, inputSize)(code, kernel, arr, dim_i, dim_j)
        total_runtime = total_runtime + runtime
        iterations = iterations + 1
        arr = output
      }
    }

    println("Time: " + total_runtime)
    println("Iterations: " + iterations)
    println(s"Output (first 20 elements): ${arr.take(20).mkString("[", ",", "]")}")
    assertArrayEquals(arr, gold, 0.01f)
  }

}
