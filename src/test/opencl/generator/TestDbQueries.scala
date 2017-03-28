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

/**
 * Some tests covering the basic features of our SQL queries.
 *
 * Each test comes with a comment describing the shape of the input data and
 * the query performed.
 *
 * Some features we don't implement here:
 * - LEFT/RIGHT JOIN: coming soon
 * - DISTINCT: it is basically what we do in `groupBy` minus the aggregation
 * - ORDER BY: it will be added once we have a `Sort` pattern
 * - HAVING: it consists in composing on the left with a filter
 */
class TestDbQueries {
  @Test def combine(): Unit = {
    /**
     * Not an SQL query.
     * Implements the operator:
     *   `combine : [a],,n,, -> [b],,m,, -> [(a,b)],,n×m,,`
     *   `combine left right = [(x, y) for x in left; y in right]`
     */
    val n = 512
    val m = 256
    val left = Array.fill(n)(util.Random.nextInt(5))
    val right = Array.fill(m)(util.Random.nextInt(5))
    
    val tuple_id = UserFun(
      "tuple_id", "x", "return x;",
      TupleType(Int, Int), TupleType(Int, Int)
    )
    
    val N = SizeVar("N")
    val M = SizeVar("M")
    
    val combine = fun(
      ArrayType(Int, N), ArrayType(Int, M),
      (left, right) => {
        Join() o MapGlb(
          toGlobal(MapSeq(tuple_id)) o fun(lRow => {
            MapSeq(fun(rRow => Tuple(lRow, rRow))) $ right
          })
        ) $ left
      }
    )
    
    val (output: Array[Int], runtime) = Execute(n)(combine, left, right)
    val gold = (for {x <- left; y <- right} yield Array(x, y)).flatten
    
    println(s"Runtime: $runtime")
    
    assertEquals(output.length, n * m * 2)
    assertArrayEquals(output, gold)
  }
  
  @Test def aggregation(): Unit = {
    /**
     * Simple aggregation.
     * table has the schema: (x INTEGER)
     *
     * Query: SELECT MAX(x) FROM table;
     */
    val size = 1024
    val table = Array.fill(size)(util.Random.nextInt(4096))
    
    val N = SizeVar("N")
    
    val max = UserFun(
      "maximum", Array("x", "y"), "return max(x, y);",
      Seq(Int, Int), Int
    )
    
    val aggregateMax = fun(
      ArrayType(Int, N),
      table => {
        toGlobal(MapSeq(idI)) o ReduceSeq(max, 0) $ table
      }
    )
    
    val (output: Array[Int], runtime) = Execute(size)(aggregateMax, table)
    
    println("SELECT MAX(x) FROM table;")
    println(s"Runtime: $runtime")
    
    assertEquals(table.max, output.head)
  }
  
  @Test def groupBy(): Unit = {
    /**
     * Aggregation using GROUP BY
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
    println(s"Runtime: $runtime")
    assertEquals(gold, output.sum)
  }
}