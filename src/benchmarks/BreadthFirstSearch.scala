//package benchmarks
//
//import ir.UserFunDef._
//import ir._
//import opencl.ir._
//
//class BreadthFirstSearch(override val name: String,
//                 override val defaultInputSizes: Seq[Int],
//                 override val delta: Float,
//                 override val f: Seq[(String, Seq[Lambda])]) extends Benchmark(name, defaultInputSizes, f, delta) {
//
//  override def runScala(inputs: Any*): Array[Float] = {
//    Array((inputs(0).asInstanceOf[Array[Float]], inputs(1).asInstanceOf[Array[Float]]).zipped.map(_*_).sum)
//  }
//
//  override def generateInputs(): Seq[Any] = {
//    val inputSize = inputSizes()(0)
//
//    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
//    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
//
//    val graph = Array.fill(inputSize, inputSize)((util.Random.nextInt(10) % 10).toFloat)
//    val initialVerticies = Array.fill(inputSize)(0.0f)
//    initialVerticies(util.Random.nextInt(inputSize)) = 1.0f
//
//    Seq(initialVerticies, initialVerticies)
//  }
//
//  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
//    val (output, runtime) = super.runOpenCL(inputs:_*)
//    (Array(output.sum), runtime)
//  }
//}
//
//object BreadthFirstSearch {
//
//  val N = Var("N")
////  val M = Var("M")
//
//  val dotProductSimple = fun(ArrayType(Float, N),
//    ArrayType(Float, N), (left, right) => {
//      Join() o MapWrg(
//        Join() o MapLcl(ReduceSeq(add, 0.0f) o MapSeq(mult)) o Split(4)
//      ) o Split(1024) $ Zip(left, right)
//    })
//
//  val denseBFSIteration = fun(
//    ArrayType(ArrayType(Float, N), N), //must be a square matrix for a graph
//    ArrayType(Float, N),
//    (graph, bfsFringe) => {
//      Join() o MapWrg(
//        MapLcl( fun( (r) => ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(bfsFringe)))
//      ) o Split(128) $ graph
//    }
//  )
//
//  val denseBFSFull = fun(
//    ArrayType(ArrayType(Float, N), N),
//    ArrayType(Float, N),
//    (graph, intialVerticies) => {
//      Iterate(N)(fun((v) => denseBFSIteration(graph, v))) $ intialVerticies
////      Iterate(N)(denseBFSIteration) $ (graph, initialVerticies)
//    }
//  )
//
//
//
//  def apply() = new BreadthFirstSearch("BFS",
//    Seq(1024),
//    0.001f,
//    Seq(("denseIteration", Seq(denseBFSIteration))))
//
//  def main(args: Array[String]) = {
//    BreadthFirstSearch().run(args)
//  }
//}
