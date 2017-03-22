package benchmarks

import ir.{TupleType, ArrayType}
import ir.ast.{Lambda, UserFun, fun, Zip}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, toGlobal}
import lift.arithmetic.SizeVar

class DBSelect(override val f: Seq[(String, Array[Lambda])])
      extends Benchmark2[(Int, Int, Int)]("SELECT", Seq(128), f, (x, y) => x == y) {
  
  override def runScala(inputs: Seq[Any]): Array[(Int, Int, Int)] = {
    val colA = inputs(0).asInstanceOf[Array[Int]]
    val colB = inputs(1).asInstanceOf[Array[Int]]
    val colC = inputs(2).asInstanceOf[Array[Int]]
    
    def is_one(n: Int): Int = if (n == 1) 1 else 0
    
    (colC.map(is_one), colA, colB).zipped.toArray
  }
  
  override def generateInputs(): Seq[Any] = {
    // Input: 3 columns (A, B, C) of integers
    val n = inputSizes().head
    
    val colA: Array[Int] = Array.tabulate(n)(i => (3*i + 4) % 10)
    val colB: Array[Int] = Array.tabulate(n)(i => (3*i + 2) % 10)
    val colC: Array[Int] = Array.tabulate(n)(i => (3*i) % 10)
   
    Seq(colA, colB, colC)
  }
  
  override def postProcessResult(variant: Int,
                                 name: String,
                                 result: Any): Array[(Int, Int, Int)] = {
    // The executor returns the result in a flattened format so we need to
    // post-process it
    val unprocessed = result.asInstanceOf[Array[Int]].grouped(3)
    unprocessed.map({ case Array(x, y, z) => (x, y, z) }).toArray
  }
  
  override protected def printParams(): Unit = {
    println("Emulating query: `SELECT A, B FROM table WHERE C = 1`")
    println("where `table` has 3 integer columns (A, B, C).")
    println("The first column of the result tells whether each row has been selected:")
    println("  1 means selected")
    println("  0 means the opposite")
  }
  
  override def buildInstanceSQLValues(variant: Int,
                                      name: String,
                                      lambdas: Array[Lambda],
                                      configuration: BenchmarkConfiguration,
                                      iStats: InstanceStatistic): String = {
    val (stats, correctness) = iStats(variant)
    stats.map(stat => s"($variant, $name, $stat, $correctness)").mkString("\n,")
  }
}

object DBSelect {
  val N = SizeVar("N")

  private val tuple_id = UserFun(
    "tuple_id", "t", "return t;",
    TupleType(Int, Int, Int), TupleType(Int, Int, Int)
  )

  private val is_one = UserFun(
    "is_one", "c", "if (c == 1) { return 1; } else { return 0; }",
    Int, Int
  )
  
  val naive: Lambda = fun(
    ArrayType(Int, N), ArrayType(Int, N), ArrayType(Int, N),
    (colA, colB, colC) => {
      MapGlb(toGlobal(tuple_id)) $ Zip(
        MapSeq(is_one) $ colC,  // The WHERE clause
        colA, colB              // The selected columns
      )
    }
  )
  
  def apply() = new DBSelect(Seq(("Naive", Array[Lambda](naive))))
  
  def main(args: Array[String]): Unit = {
    DBSelect().run(args)
  }
}
