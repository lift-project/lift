package opencl.generator

import ir.ast.{FunCall, Gather, Get, Join, Scatter, Split, Tuple, Unzip, UserFun, Zip, fun, shiftRight}
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.{AfterClass, BeforeClass, Test}

object TestDbQueries {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }
  
  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestDbQueries {
  @Test def combine(): Unit = {
    val size = 512
    val left = Array.fill(size)(util.Random.nextInt(5))
    val right = Array.fill(size)(util.Random.nextInt(5))
    
    val tuple_id = UserFun(
      "tuple_id", "x", "return x;",
      TupleType(Int, Int), TupleType(Int, Int)
    )
    
    val N = SizeVar("N")
    
    // [Int],,n,, -> [Int],,n,, -> [(Int, Int)],,n^2,,
    val combine = fun(
      ArrayType(Int, N), ArrayType(Int, N),
      (left, right) => {
        Join() o MapGlb(
          toGlobal(MapSeq(tuple_id)) o fun(lRow => {
            MapSeq(fun(rRow => Tuple(lRow, rRow))) $ right
          })
        ) $ left
      }
    )
    
    // Note: `output` is flattened
    val (output: Array[Int], runtime) = Execute(size)(combine, left, right)
    val gold = (for {x <- left; y <- right} yield Array(x, y)).flatten
    
    println(s"Runtime: $runtime")
    
    assertEquals(output.length, size * size * 2)
    assertArrayEquals(output, gold)
  }
  
  @Test def aggregation(): Unit = {
    val size = 1024
    val table = Array.fill(size)(util.Random.nextInt(4096))
    
    val N = SizeVar("N")
    
    val max = UserFun(
      "maximum", Array("x", "y"), "return max(x, y);",
      Seq(Int, Int), Int
    )
    
    // SELECT MAX(x) FROM xs;
    val aggregateMax = fun(
      ArrayType(Int, N),
      table => {
        toGlobal(MapSeq(idI)) o ReduceSeq(max, 0) $ table
      }
    )
    
    val (output: Array[Int], runtime) = Execute(size)(aggregateMax, table)
    
    println(s"Runtime: $runtime")
    
    assertEquals(table.max, output.head)
  }
  
  @Test def groupBy(): Unit = {
    /**
     * table has the schema: (x INTEGER, y INTEGER)
     *
     * Query: SELECT x, SUM(y) FROM table GROUP BY x
     */
    val size = 1024
    val table = Array.fill(size, 2)(util.Random.nextInt(10)).sortBy(_(0))
    
    val N = SizeVar("N")
    
    val fst = UserFun(
      "fst", Array("x", "y"), "return x;", Seq(Int, Int), Int
    )
    
    val tupleId = UserFun(
      "tupleId", "t", "return t;",
      TupleType(Int, Int, Int), TupleType(Int, Int, Int)
    )
    
    val eq = UserFun(
      "eq", Array("x", "y"), "return x != y;", Seq(Int, Int), Int
    )
    
    val filterOnX = UserFun(
      "filterOnX", Array("x_ref", "row"), "return (row._0 == x_ref) ? row._1 : 0;",
      Seq(Int, TupleType(Int, Int)), Int
    )
  
    // We assume that the input table is already sorted
    val groupBy = fun(
      ArrayType(TupleType(Int, Int), N),
      table => {
        Join() o MapGlb(
          fun(bx =>
            toGlobal(MapSeq(tupleId)) o
            MapSeq(fun(tot => Tuple(bx._0, bx._1, tot))) o
            ReduceSeq(addI, 0) o
            MapSeq(filterOnX) o
            MapSeq(fun(row => Tuple(bx._1, row))) $ table
          )
        ) $ Zip(
          MapSeq(eq) o fun(xs => Zip(
            xs,
            // The trick: filter (==) zip(xs, shiftRight xs))
            MapSeq(idI) o Gather(shiftRight) o MapSeq(idI) $ xs)) o MapSeq(fst) $ table,
          MapSeq(fst) $ table
        )
      }
    )
    
    val (unprocessedOutput: Array[Int], runtime) = Execute(size)(groupBy, table.flatten)
    // We will not need to perform this post-processing pass once we have a Filter pattern
    val output: Array[Int] = unprocessedOutput
      .grouped(3)
      .filter(_(0) == 1)
      .map(_.slice(1, 3))
      .toArray
      .flatten
    
    val gold: Array[Int] = table
      .groupBy(_(0))
      .mapValues(v => v.map(_ (1)).sum)
      .toArray
      .map(r => Array(r._1, r._2))
      .sortBy(_(0))
      .flatten
    
    println("SELECT x, SUM(y) FROM table GROUP BY x;")
    println(s"Runtime: $runtime")
    assertArrayEquals(gold, output)
  }
  
  @Test def complexQuery(): Unit = {
    /**
     * leftTable has the schema: (a INTEGER, b INTEGER, c INTEGER)
     * rightTable has the schema: (x INTEGER, y INTEGER)
     *
     * Query: SELECT SUM(c)
     *        FROM left INNER JOIN right ON a = x
     *        WHERE b + y > 10
     */
    val n = 2048
    val m = 512
    val leftTable = Array.fill(n, 3)(util.Random.nextInt(10))
    val rightTable = Array.fill(m, 2)(util.Random.nextInt(10))
    
    val N = SizeVar("N")
    val M = SizeVar("M")
  
    val joinTuples = UserFun(
      "concat", Array("x", "y"),
      "Tuple_int_int_int_int t = {(x._0 == y._0), x._1, x._2, y._1}; return t;",
      Seq(TupleType(Int, Int, Int), TupleType(Int, Int)),
      TupleType(Int, Int, Int, Int)
    )
    
    val where_clause = UserFun(
      "where_clause", "t",
      "Tuple_int_int row = {(t._0 && ((t._1 + t._3) > 10)), t._2};\n return row;",
      TupleType(Int, Int, Int, Int), TupleType(Int, Int)
    )
    
    val query = fun(
      ArrayType(TupleType(Int, Int, Int), N),
      ArrayType(TupleType(Int, Int), M),
      (leftTable, rightTable) => {
        MapSeq(toGlobal(idI)) o ReduceSeq(addI, 0) o Join() o
        MapWrg(
          MapLcl(toGlobal(idI)) o ReduceSeq(addI, 0) o Join() o
          MapLcl(MapSeq(toLocal(idI)) o ReduceSeq(addI, 0)) o
          toLocal(MapLcl(
            MapSeq(multI) o
            MapSeq(where_clause) o
            fun(lRow => {
              MapSeq(joinTuples) o
              MapSeq(fun(rRow => Tuple(lRow, rRow))) $ rightTable
            })
          ))
        ) o Split(2) $ leftTable
      }
    )
    
    val gold = { for (Array(a, b, c) <- leftTable;
                      Array(x, y) <- rightTable
                      if a == x && b + y > 10) yield c}.sum
  
  val (output: Array[Int], runtime) =
    Execute(n)(query, leftTable.flatten, rightTable.flatten)
  
    println("SELECT SUM(left.c) " +
      "FROM left INNER JOIN right ON left.a = right.x " +
      "WHERE left.b + right.y > 10;")
    println(s"Output length: ${output.length}")
    println(s"Runtime: $runtime")
    assertEquals(gold, output.sum)
  }
}