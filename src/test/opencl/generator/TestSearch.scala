package opencl.generator

import apart.arithmetic.Var
import ir._
import ir.ast._
import ir.ast.UserFun._
import opencl.executor._
import opencl.ir._
import opencl.ir.ast._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}

import opencl.ir.pattern._

object TestSearch {
  @BeforeClass def TestSearch() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}



class TestSearch {
  val t_id = UserFun("tuple_id", "x", "return x;", TupleType(Int, Int), TupleType(Int, Int))
  val i_id = UserFun("int_id", "x", "return x;", Int, Int)
  val int_add = UserFun("int_add", Array("a", "b"), "return a+b;", Array(Int, Int), Int);

  @Test def SCALAR_BINARY_SEARCH() : Unit = {
     val inputSize = Math.pow(2, 12).toInt
     val search_arr = Array.tabulate(inputSize)((i:Int) => i)
     val search_index = util.Random.nextInt(inputSize)
     val gold = search_arr(search_index)
     // compare: compare the search variable s, with the indexed element i
     val compare = UserFun("comp", Array("elem", "index"), "return (index-elem);", Array(Int, Int), Int)
     val N = Var("N")
     val searchKernel = fun(
       ArrayType(Int, N),
       ArrayType(Int, 1),
       (array, ixarr) => {
         MapSeq(toGlobal(i_id)) o Join() o MapSeq(
           toGlobal(fun((ix) =>
             BSearch(toPrivate(fun((elem) => compare.apply(elem, ix))), 0) $ array
           ))
         ) $ ixarr
       }
     )
     val (output:Array[Int], runtime) = Execute(1,1, (true, true))(searchKernel, search_arr, Array(search_index))
     println("Search Index: " + search_index)
     println("Gold: "+gold)
     println("Result: "+output(0))
     println("Time: " + runtime)
     println("Running!")
     assert(output(0) == gold)
  }

  @Test def SCALAR_LINEAR_SEARCH() : Unit = {
     val inputSize = Math.pow(2, 12).toInt
     val search_arr = Array.tabulate(inputSize)((i:Int) => i)
     val search_index = util.Random.nextInt(inputSize)
     val gold = search_arr(search_index)
     // compare: compare the search variable s, with the indexed element i
     val compare = UserFun("comp", Array("elem", "index"), "return (index-elem);", Array(Int, Int), Int)
     val N = Var("N")
     val searchKernel = fun(
       ArrayType(Int, N),
       ArrayType(Int, 1),
       (array, ixarr) => {
         MapSeq(toGlobal(i_id)) o Join() o MapSeq(
           toGlobal(fun((ix) =>
             LSearch(toPrivate(fun((elem) => compare.apply(elem, ix))), 0) $ array
           ))
         ) $ ixarr
       }
     )
     val (output:Array[Int], runtime) = Execute(1,1, (true, true))(searchKernel, search_arr, Array(search_index))
     println("Search Index: " + search_index)
     println("Gold: "+gold)
     println("Result: "+output(0))
     println("Time: " + runtime)
     println("Running!")
     assert(output(0) == gold)
  }

  @Ignore @Test def SPLIT_SEARCH() : Unit = {
    // test of splitting an array, mapping a search over each element, then searching the results
    val inputSize = Math.pow(2, 12).toInt
     val search_arr = Array.tabulate(inputSize)((i:Int) => i)
     val search_index = util.Random.nextInt(inputSize)
     val gold = search_arr(search_index)
     // compare: compare the search variable s, with the indexed element i
     val compare = UserFun("comp", Array("elem", "index"), "return (index-elem);", Array(Int, Int), Int)
     val N = Var("N")
     val searchKernel = fun(
       ArrayType(Int, N),
       ArrayType(Int, 1),
       (array, ixarr) => {
         MapSeq(toGlobal(i_id)) o Join() o MapSeq(
           fun((ix) =>
             LSearch(toPrivate(fun((elem) => compare.apply(elem, ix))), 0) o Join() o MapSeq(
              ReduceSeq(int_add, 0) o Join() o MapSeq(
                fun((subarr) =>
                  LSearch(toPrivate(fun((elem) => compare.apply(elem, ix))), 0) $ subarr
                )
              )
             ) o Split(8) $ array
           )
         ) $ ixarr
       }
     )
     val (output:Array[Int], runtime) = Execute(1,1, (true, true))(searchKernel, search_arr, Array(search_index))
     println("Search Index: " + search_index)
     println("Gold: "+gold)
     println("Result: "+output(0))
     println("Time: " + runtime)
     assert(output(0) == gold)
  }

  @Ignore @Test def NESTED_BINARY_SEARCH() : Unit = {
    val inputSize = Math.pow(2, 4).toInt
    // 2d array of elements to search through
    val search_arrs = Array.tabulate(inputSize)((i:Int) => Array.tabulate(inputSize)((j:Int) => j))
    // indicies to search for - chosen randomly
    val search_indices = Array.tabulate(inputSize)((i:Int) => util.Random.nextInt(inputSize))
    // the gold result
    val gold = search_indices.zip(search_arrs).map({ case (i:Int, arr:Array[Int]) => arr(i)})
    // comparison function
    val compare = UserFun("comp", Array("s", "i"), "return (i-s);", Array(Int, Int), Int)
    val N = Var("N")
    val searchKernel = fun(
      ArrayType(Int, N), //indicies
      ArrayType(ArrayType(Int, N), N), //search arrays
      (ixs, arrs) => {
          MapGlb(fun((i_arr_p) =>
            MapSeq(toGlobal(i_id)) o BSearch(toPrivate(fun((elem) => compare.apply(elem, Get(i_arr_p, 0)))), 0) $ Get(i_arr_p, 1)
          )
        ) $ Zip(ixs, arrs) // pair indicies with arrays to search
      }
    )
    val (output: Array[Int], runtime) = Execute(inputSize,inputSize)(searchKernel, search_indices, search_arrs)
    println("Search Indicies: [" + search_indices.deep.mkString(",") + "]")
    println("Gold: ["+gold.deep.mkString(",") + "]")
    println("Result: ["+output.deep.mkString(",") + "]")
    println("Time: " + runtime)
    assertArrayEquals(gold, output)
  }

   @Ignore @Test def NESTED_LINEAR_SEARCH() : Unit = {
    val inputSize = Math.pow(2, 4).toInt
    // 2d array of elements to search through
    val search_arrs = Array.tabulate(inputSize)((i:Int) => Array.tabulate(inputSize)((j:Int) => j))
    // indicies to search for - chosen randomly
    val search_indices = Array.tabulate(inputSize)((i:Int) => util.Random.nextInt(inputSize))
    // the gold result
    val gold = search_indices.zip(search_arrs).map({ case (i:Int, arr:Array[Int]) => arr(i)})
    // comparison function
    val compare = UserFun("comp", Array("s", "i"), "return (i-s);", Array(Int, Int), Int)
    val N = Var("N")
    val searchKernel = fun(
      ArrayType(Int, N), //indicies
      ArrayType(ArrayType(Int, N), N), //search arrays
      (ixs, arrs) => {
          MapGlb(fun((i_arr_p) =>
            MapSeq(toGlobal(i_id)) o LSearch(toPrivate(fun((elem) => compare.apply(elem, Get(i_arr_p, 0)))), 0) $ Get(i_arr_p, 1)
          )
        ) $ Zip(ixs, arrs) // pair indicies with arrays to search
      }
    )
    val (output: Array[Int], runtime) = Execute(inputSize,inputSize)(searchKernel, search_indices, search_arrs)
    println("Search Indicies: [" + search_indices.deep.mkString(",") + "]")
    println("Gold: ["+gold.deep.mkString(",") + "]")
    println("Result: ["+output.deep.mkString(",") + "]")
    println("Time: " + runtime)
    assertArrayEquals(gold, output)
  }

  @Test def TUPLE_BINARY_SEARCH() : Unit = {
    val inputSize = Math.pow(2, 4).toInt
    val search_arr = Array.tabulate(inputSize)((i:Int) => (i, util.Random.nextInt(100)))
    val flat_arr = search_arr.map((t) => Array(t._1, t._2)).flatten
    val search_index = (util.Random.nextInt(inputSize), -1)
    val flat_search_index = Array(search_index._1, search_index._2)
    val gold = search_arr(search_index._1)._2

    val compare = UserFun("comp", Array("current_index", "search_index"),
    """
      |if(search_index._0 < current_index._0) {
      | if(search_index._0 == -1) {
      |   // search index is -1, need to move right
      |   // this should never happen
      |   return 1;
      | } else {
      |   // search index less, move left
      |   return -1;
      | }
      |} else if(current_index._0 < search_index._0) {
      | if(current_index._0 == -1) {
      |   // current index is -1, we should move it left
      |   return -1;
      | }else{
      |   // current index is less, we need to move right
      |   return 1;
      | }
      |} else {
      | // the indicies match - don't move!
      | return 0;
      |}
    """.stripMargin,
      Array(TupleType(Int, Int), TupleType(Int, Int)),
      Int // The direction we should move...
    )

    val N = Var("N")
    val searchKernel = fun(
      ArrayType(TupleType(Int, Int), N),
      ArrayType(TupleType(Int, Int), 1),
      (array, ixarr) => {
        MapSeq(toGlobal(t_id)) o Join() o MapSeq(
           fun((ix) =>
             BSearch(toPrivate(fun((elem) => compare.apply(elem, ix))), (0, 0)) $ array
           )
         ) $ ixarr
      }
    )

    val (output:Array[Int], runtime) = Execute(1,1)(searchKernel, flat_arr, flat_search_index)
    println("Search Index: "+search_index)
    println("Gold: " + gold)
    println("Result: " + output(0))
    println("Time: " + runtime)
  }

  @Test def TUPLE_LINEAR_SEARCH() : Unit = {
    val inputSize = Math.pow(2, 4).toInt
    val search_arr = Array.tabulate(inputSize)((i:Int) => (i, util.Random.nextInt(100)))
    val flat_arr = search_arr.map((t) => Array(t._1, t._2)).flatten
    val search_index = (util.Random.nextInt(inputSize), -1)
    val flat_search_index = Array(search_index._1, search_index._2)
    val gold = search_arr(search_index._1)._2

    val compare = UserFun("comp", Array("current_index", "search_index"),
    """
      |if(search_index._0 < current_index._0) {
      | if(search_index._0 == -1) {
      |   // search index is -1, need to move right
      |   // this should never happen
      |   return 1;
      | } else {
      |   // search index less, move left
      |   return -1;
      | }
      |} else if(current_index._0 < search_index._0) {
      | if(current_index._0 == -1) {
      |   // current index is -1, we should move it left
      |   return -1;
      | }else{
      |   // current index is less, we need to move right
      |   return 1;
      | }
      |} else {
      | // the indicies match - don't move!
      | return 0;
      |}
    """.stripMargin,
      Array(TupleType(Int, Int), TupleType(Int, Int)),
      Int // The direction we should move...
    )

    val N = Var("N")
    val searchKernel = fun(
      ArrayType(TupleType(Int, Int), N),
      ArrayType(TupleType(Int, Int), 1),
      (array, ixarr) => {
        MapSeq(toGlobal(t_id)) o Join() o MapSeq(
           fun((ix) =>
             LSearch(toPrivate(fun((elem) => compare.apply(elem, ix))), (0, 0)) $ array
           )
         ) $ ixarr
      }
    )

    val (output:Array[Int], runtime) = Execute(1,1)(searchKernel, flat_arr, flat_search_index)
    println("Search Index: "+search_index)
    println("Gold: " + gold)
    println("Result: " + output(0))
    println("Time: " + runtime)
  }
}
