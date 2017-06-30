package opencl.generator

import ir.ast.{Gather, Join, Split, Tuple, UserFun, Zip, fun, shiftRight}
import ir.{ArrayTypeWSWC, TupleType}
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
 * Some features we don't explicitly implement here:
 * - LEFT/RIGHT JOIN: partially implemented, we need to be able to pad some
 *                    constants
 * - DISTINCT: it is basically what we do in `groupBy` minus the aggregation
 * - ORDER BY: it will be added once we have a `Sort` pattern
 * - HAVING: it consists in composing on the left with a filter
 */
class TestDbQueries {
  /** Because the executor can't return tuple of type (bool, int) */
  val toInt = UserFun("to_int", "x", "return x;", Bool, Int)
  
  @Test def combine(): Unit = {
    /**
     * Not an SQL query.
     * Implements the operator:
     *   `combine : [a],,n,, -> [b],,m,, -> [(a,b)],,n×m,,`
     *   `combine left right = [(x, y) for x in left; y in right]`
     */
    val n = 128
    val m = 256
    val left = Array.fill(n)(util.Random.nextInt(5))
    val right = Array.fill(m)(util.Random.nextInt(5))
  
    val N = SizeVar("N")
    val M = SizeVar("M")
  
    val tupleId = id(TupleType(Int, Int))
    
    val combine = fun(
      ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int, M),
      (left, right) => {
        Join() o MapGlb(
          toGlobal(MapSeq(tupleId)) o fun(lRow => {
            MapSeq(fun(rRow => Tuple(lRow, rRow))) $ right
          })
        ) $ left
      }
    )
    
    val (output: Array[Int], runtime) = Execute(n)(combine, left, right)
    val gold = (for {x <- left; y <- right} yield Array(x, y)).flatten
    
    println("combine left right")
    println(s"Runtime: $runtime")
    
    assertEquals(output.length, n * m * 2)
    assertArrayEquals(output, gold)
  }
  
  @Test def leftJoin(): Unit = {
    /**
     * This example does not implement a LEFT JOIN but shows how to work on
     * the "left" part of a LEFT JOIN.
     *
     * leftTable has the schema: (a INT, b: INT)
     * rightTable has the schema: (x, INT, y: INT)
     *
     * Query: SELECT a, b
     *        FROM leftTable LEFT JOIN rightTable ON a = x
     *        WHERE y IS NULL;
     */
    val n = 128
    val m = 128
    val leftTable = Array.fill(n, 2)(util.Random.nextInt(10))
    val rightTable = Array.fill(m, 2)(util.Random.nextInt(8))
    
    val N = SizeVar("N")
    val M = SizeVar("M")
    
    val sameKeys = UserFun(
      "sameKeys", Array("x", "y"),
      "return (x._0 == y._0);",
      Seq(TupleType(Int, Int), TupleType(Int, Int)), Bool
    )
    
    val toTuple = UserFun(
      "toTuple", Array("is_bound", "row"),
      "Tuple1 t = {is_bound, row._0, row._1}; return t;",
      Seq(Int, TupleType(Int, Int)), TupleType(Int, Int, Int)
    )
    
    val query = fun(
      ArrayTypeWSWC(TupleType(Int, Int), N),
      ArrayTypeWSWC(TupleType(Int, Int), M),
      (left, right) => {
        Join() o MapGlb(fun(lRows =>
          MapSeq(toGlobal(toTuple)) $ Zip(
            Join() o MapSeq(MapSeq(toInt o not) o ReduceSeq(or, false)) o
              MapSeq(fun(lRow => {
                MapSeq(fun(rRow => sameKeys.apply(lRow, rRow))) $ right
              })) $ lRows,
            lRows
          )
        )) o Split(4) $ left
      }
    )
    
    val (output: Array[Int], runtime) =
      Execute(n)(query, leftTable.flatten, rightTable.flatten)
    
    val gold: Array[Int] = leftTable.flatMap(row =>
      Array(if (rightTable.exists(_(0) == row(0))) 0 else 1,
            row(0),
            row(1))
    )
    
    println("SELECT a, b FROM leftTable LEFT JOIN rightTable ON a = x " +
            "WHERE y IS NULL")
    println(s"Runtime: $runtime")
  
    assertArrayEquals(gold, output)
  }
  
  @Test def aggregation(): Unit = {
    /**
     * Simple aggregation.
     * table has the schema: (x INT)
     *
     * Query: SELECT MAX(x) FROM table;
     */
    val size = 1024
    val table = Array.fill(size)(util.Random.nextInt(4096))
    
    val N = SizeVar("N")
    
    val int_max = max(Int)
    
    val aggregateMax = fun(
      ArrayTypeWSWC(Int, N),
      table => {
        toGlobal(MapSeq(idI)) o ReduceSeq(int_max, 0) $ table
      }
    )
    
    val (output: Array[Int], runtime) = Execute(size)(aggregateMax, table)
    
    println("SELECT MAX(x) FROM table")
    println(s"Runtime: $runtime")
    
    assertEquals(table.max, output.head)
  }
  
  @Test def groupBy(): Unit = {
    /**
     * Aggregation using GROUP BY
     * table has the schema: (x INT, y INT)
     *
     * Query: SELECT x, SUM(y) FROM table GROUP BY x
     */
    val size = 1024
    val table = Array.fill(size, 2)(util.Random.nextInt(10)).sortBy(_(0))
    
    val N = SizeVar("N")
    
    val int_fst = first(Int, Int)
    val tuple_id = id(TupleType(Int, Int, Int), "tuple_id")
    val int_eq = equality(Int)
    
    val filterOnX = UserFun(
      "filterOnX", Array("x_ref", "row"), "return (row._0 == x_ref) ? row._1 : 0;",
      Seq(Int, TupleType(Int, Int)), Int
    )
  
    // We assume that the input table is already sorted
    val groupBy = fun(
      ArrayTypeWSWC(TupleType(Int, Int), N),
      table => {
        Join() o MapGlb(
          fun(bx =>
            toGlobal(MapSeq(tuple_id)) o
            MapSeq(fun(tot => Tuple(toInt $ bx._0, bx._1, tot))) o
            ReduceSeq(addI, 0) o
            MapSeq(filterOnX) o
            MapSeq(fun(row => Tuple(bx._1, row))) $ table
          )
        ) $ Zip(
          MapSeq(not o int_eq) o fun(xs => Zip(
            xs,
            // The trick: filter (==) zip(xs, shiftRight xs))
            MapSeq(idI) o Gather(shiftRight) o MapSeq(idI) $ xs)) o MapSeq(int_fst) $ table,
          MapSeq(int_fst) $ table
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
    
    println("SELECT x, SUM(y) FROM table GROUP BY x")
    println(s"Runtime: $runtime")
    assertArrayEquals(gold, output)
  }
  
  @Test def complexQuery(): Unit = {
    /**
     * leftTable has the schema: (a INT, b INT, c INT)
     * rightTable has the schema: (x INT, y INT)
     *
     * Query: SELECT SUM(c)
     *        FROM leftTable INNER JOIN rightTable ON a = x
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
      "Tuple2 t = {(x._0 == y._0), x._1, x._2, y._1}; return t;",
      Seq(TupleType(Int, Int, Int), TupleType(Int, Int)),
      TupleType(Int, Int, Int, Int)
    )
    
    val where_clause = UserFun(
      "where_clause", "t",
      "Tuple1 t2 = {(t._0 && ((t._1 + t._3) > 10)), t._2}; return t2;",
      TupleType(Int, Int, Int, Int), TupleType(Int, Int)
    )
    
    val query = fun(
      ArrayTypeWSWC(TupleType(Int, Int, Int), N),
      ArrayTypeWSWC(TupleType(Int, Int), M),
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
  
    println("SELECT SUM(c) FROM leftTable INNER JOIN rightTable ON a = x " +
            "WHERE b + y > 10")
    println(s"Runtime: $runtime")
    assertEquals(gold, output.sum)
  }
}