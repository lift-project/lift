package benchmarks

import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import ir.ast.{Join, Lambda, Split, Tuple, UserFun, Zip, fun}
import opencl.ir._
import opencl.ir.pattern._
import lift.arithmetic.SizeVar

class DbSelect(override val f: Seq[(String, Array[Lambda])])
      extends Benchmark[Vector[(Int, Int, Int)]]("SELECT", Seq(4096), f, _ == _) {
  
  override def runScala(inputs: Seq[Any]): Vector[(Int, Int, Int)] = {
    val colA = inputs(0).asInstanceOf[Array[Int]]
    val colB = inputs(1).asInstanceOf[Array[Int]]
    val colC = inputs(2).asInstanceOf[Array[Int]]
    
    def is_one(n: Int): Int = if (n == 1) 1 else 0
    
    (colC.map(is_one), colA, colB).zipped.toVector
  }
  
  override def generateInputs(): Seq[Any] = {
    // Input: 3 columns (A, B, C) of integers
    val n = inputSizes().head
    
    val colA: Array[Int] = Array.tabulate(n)(i => (3*i + 4) % 10)
    val colB: Array[Int] = Array.tabulate(n)(i => (3*i + 2) % 10)
    val colC: Array[Int] = Array.tabulate(n)(i => (3*i) % 10)
   
    Seq(colA, colB, colC)
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

object DbSelect {
  private val N = SizeVar("N")

  private val tuple_id = id(TupleType(Int, Int, Int))

  private val is_one = UserFun(
    "is_one", "c", "if (c == 1) { return 1; } else { return 0; }",
    Int, Int
  )
  
  val naive: Lambda = fun(
    ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int, N),
    (colA, colB, colC) => {
      MapGlb(toGlobal(tuple_id)) $ Zip(
        MapSeq(is_one) $ colC,  // The WHERE clause
        colA, colB              // The selected columns
      )
    }
  )
  
  val divideNConquer: Lambda = fun(
    ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int, N),
    (colA, colB, colC) => {
      Join() o MapWrg(
        Join() o MapLcl(MapSeq(
          fun(x => tuple_id $ Tuple(is_one $ x._2, x._0, x._1))
        )) o Split(4)
      ) o Split(256) $ Zip(colA, colB, colC)
    }
  )
  
  def apply() = new DbSelect(Seq(
    ("Naive", Array[Lambda](naive)),
    ("Divide and conquer", Array[Lambda](divideNConquer))
  ))
  
  def main(args: Array[String]): Unit = {
    DbSelect().run(args)
  }
}
