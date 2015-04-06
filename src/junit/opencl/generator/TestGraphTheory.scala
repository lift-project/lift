package junit.opencl.generator

import ir.UserFunDef._
import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._
import sys.process._
import java.io._

object TestGraphTheory {
  @BeforeClass def TestMatrixBasic() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}


class TestGraphTheory {


  val add = UserFunDef("add", Array("a","b"), "return a+b;", Seq(Float, Float), Float)
  val mult = UserFunDef("mult", Array("a","b"), "return a*b;", Seq(Float, Float), Float)

  def DENSE_BFS_ITERATION(): Unit = {
    val inputSize = 1024
    val graph = Array.tabulate(inputSize, inputSize)((r:Int,c:Int) => (if(util.Random.nextInt(100)>2) 0 else 1).toFloat)
    var fringe = Array.fill(inputSize)(0.0f)
    fringe(util.Random.nextInt(inputSize)) = 1.0f

    val N = Var("N")
    val denseBFSIteration = fun(
      ArrayType(ArrayType(Float, N), N), //must be a square matrix for a graph
      ArrayType(Float, N),
      (graph, bfsFringe) => {
        fun((fr) =>
        Join() o MapWrg(
          Join() o MapLcl( fun( (r) => ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(fr,r)))
        ) o Split(128) $ graph
        ) $ bfsFringe
      }
    )
    val (output, runtime) = Execute(inputSize*inputSize)(denseBFSIteration, graph, fringe, inputSize)
    println("output(0) = " + output(0))
    println(fringe.toList)
    println(output.toList)
    println("runtime = " + runtime)
    assertArrayEquals(scalaBFSIteration(graph,fringe), output, 0.0f)
  }

  def DENSE_BFS_ITERATION_FIXED_SIZE(): Unit = {
    val inputSize = 1024
    val graph = Array.tabulate(inputSize, inputSize)((r:Int,c:Int) => (if(util.Random.nextInt(100)>2) 0 else 1).toFloat)
    var fringe = Array.fill(inputSize)(0.0f)
    fringe(util.Random.nextInt(inputSize)) = 1.0f

    val denseBFSIteration = fun(
      ArrayType(ArrayType(Float, 1024), 1024), //must be a square matrix for a graph
      ArrayType(Float, 1024),
      (graph, bfsFringe) => {
        Join() o MapWrg(
          MapLcl( fun( (r) => ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(bfsFringe,r)))
        ) o Split(128) $ graph
      }
    )
    val (output, runtime) = Execute(inputSize*inputSize)(denseBFSIteration, graph, fringe)
    println("output(0) = " + output(0))
    println(fringe.toList)
    println(output.toList)
    println("runtime = " + runtime)
//    printDFSDotFile(graph, output)
    assertArrayEquals(scalaBFSIteration(graph,fringe), output, 0.0f)

  }

  def DENSE_BFS_MULTI_ITERATION(): Unit = {
    val inputSize = 1024
    val graphArr = Array.tabulate(inputSize, inputSize)((r:Int,c:Int) => (if(util.Random.nextInt(10)>2) 0 else 1).toFloat)
    var fringeArr = Array.fill(inputSize)(0.0f)
    fringeArr(util.Random.nextInt(inputSize)) = 1.0f
    val N = Var("N")


    val or = UserFunDef("or", Array("a","b"), "return (((a>0.0f)||(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)
    val and = UserFunDef("and", Array("a","b"), "return (((a>0.0f)&&(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)


    val BFSMultiIteration  = fun(
      ArrayType(ArrayType(Float, N), N), //must be a square matrix for a graph
    ArrayType(Float, N),
    (graph, bfsFringe) => {
//      Iterate(100)(Join() o fun((fr) => denseBFSIteration(graph, fr))) $ bfsFringe
      Iterate(10)( fun((fr) =>
        Join() o MapWrg(
          Join() o MapLcl( fun( (r) => ReduceSeq(and, 0.0f) o MapSeq(or) $ Zip(fr,r)))
        ) o Split(128) $ graph
      )) $ bfsFringe
    })

    val (output, runtime) = Execute(inputSize*inputSize)(BFSMultiIteration, graphArr, fringeArr,inputSize)
    println("output(0) = " + output(0))
    println(fringeArr.toList)
    println(output.toList)
    println("runtime = " + runtime)
    val gold = scalaIterateBFS(10,graphArr,fringeArr)
    assertArrayEquals(gold, output, 0.0f)

  }

  def DENSE_BFS_MULTI_ITERATION_FIXED_SIZE() : Unit = {
    val inputSize = 16
    val graphArr = Array.tabulate(inputSize, inputSize)((r:Int,c:Int) => (if(util.Random.nextInt(25)>2) 0 else 1).toFloat)
    var fringeArr = Array.fill(inputSize)(1.0f)
    fringeArr(util.Random.nextInt(inputSize)) = 1.0f
    val N = Var("N")
    val or = UserFunDef("or", Array("a","b"), "return (((a>0.0f)||(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)
    val and = UserFunDef("and", Array("a","b"), "return (((a>0.0f)&&(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)

    val BFSMultiIteration  = fun(
      ArrayType(ArrayType(Float, 16), 16), //must be a square matrix for a graph
      ArrayType(Float, 16),
      (graph, bfsFringe) => {
        Iterate(7)( fun((fr) =>
//          Join() o Join() o MapWrg(
          Join() o MapSeq( fun( (r) => ReduceSeq(mult, 0.0f) o MapSeq(add) $ Zip(fr,r))) $ graph
//          ) o Split(128) $ graph
        )) $ bfsFringe
      })

    val (output, runtime) = Execute(inputSize*inputSize)(BFSMultiIteration, graphArr, fringeArr)
    val gold = scalaIterateBFS(7,graphArr,fringeArr)
    println("output(0) = " + output(0))
    println("FringArr = " + fringeArr.toList.toString())
    println("Output = " + output.toList.toString())
    println("OSum = " + output.reduce(_+_))
    println("Gold = " + gold.toList.toString())
    println("GSum = " + gold.reduce(_+_))
    println("runtime = " + runtime)
    printDFSDotFile(graphArr, output, gold, fringeArr)
    assertArrayEquals(gold,output, 0.0f)
  }

  @Test def DENSE_MULTI_ITERATION_MWE() : Unit = {
    val inputSize = 32
    val matrix = Array.tabulate(inputSize, inputSize)((r:Int,c:Int) => if(r==c) 1.1f else 0.0f)
    val vector = Array.fill(inputSize)(1.0f)

//    val N =

    val DotProductMultiIteration  = fun(
      ArrayType(ArrayType(Float, 32), 32), //must be a square matrix for a graph
      ArrayType(Float, 32),
//    ScalarType("N", Var("N")),
      (matrix, vector) => {
        Iterate(10)( fun((v) =>
//          Join() o MapWrg( Join() o MapLcl( fun ( (r) => ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(v,r)))) o Split(4) $ matrix
          Join() o MapSeq( fun ( (r) => ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(v,r))) $ matrix
        )) $ vector
      })

    val kernelCode =
    """float mult(float a, float b) {return a*b;}
      |float add(float a, float b) {return a+b;}
      |
      |void iterationBody(global float* v_tin_25, global float* v_tout_26, global float* v__4, global float* v__10) {
      | /* map_seq */
      |      for (int v_i_3 = 0; v_i_3 < 32; v_i_3 += 1) {
      |        /* map_seq */
      |        for (int v_i_2 = 0; v_i_2 < 32; v_i_2 += 1) {
      |          *((global float*)&v__10[(v_i_2 + (32 * v_i_3))]) = mult(*((global float*)&v_tin_25[v_i_2]), *((global float*)&v__4[(v_i_2 + (32 * v_i_3))]));
      |        //mem_fence(CLK_GLOBAL_MEM_FENCE);
      |        }
      |        mem_fence(CLK_GLOBAL_MEM_FENCE);
      |        /* map_seq */
      |        {
      |          /* reduce_seq */
      |          float v__11 = 0.0f;
      |          for (int v_i_1 = 0; v_i_1 < 32; v_i_1 += 1) {
      |            v__11 = add(v__11, *((global float*)&v__10[(v_i_1 + (32 * v_i_3))]));
      |        //mem_fence(CLK_GLOBAL_MEM_FENCE);
      |          }
      |          *((global float*)&v_tout_26[v_i_3]) = v__11;
      |        //mem_fence(CLK_GLOBAL_MEM_FENCE);
      |
      |          /* reduce_seq */
      |        }
      |      }
      |      //mem_fence(CLK_GLOBAL_MEM_FENCE);
      |      /* map_seq */
      |
      |}
      |
      |kernel void KERNEL(global float* v__5, global float* v__14, global float* v__7, global float* v__4, global float* v__10) {
      |  {
      |    int v_curOutLen_23 = 6;
      |    volatile global float* v_tin_25 = v__5;
      |    volatile global float* v_tout_26 = ( (10 & 1) != 0 ) ? v__14 : v__7 ;
      |    #pragma unroll 1
      |    for (int v_i_24 = 0; v_i_24 < 10; v_i_24 += 1) {
      |      iterationBody(v_tin_25, v_tout_26,v__4, v__10);
      |      v_tin_25 = ( v_tout_26==v__7 ) ? v__7:v__14;
      |      v_tout_26 = ( v_tout_26==v__7 ) ? v__14:v__7;
      |    }
      |  }
      |}
      |""".stripMargin
//    mem_fence(CLK_GLOBAL_MEM_FENCE);

    Compile(DotProductMultiIteration)

    val (output, runtime) = Execute(1, 1)(kernelCode, DotProductMultiIteration, matrix, vector)


    println("output(0) = " + output(0))
    println("FringeArr = " + vector.toList.toString())
    println("Output = " + output.toList.toString())

  }

  def scalaIterateDotProduct(iterations: Int,matrix:Array[Array[Float]],vector:Array[Float]) : Array[Float] = {
    var tVector = vector
    for(i:Int <- 0 until iterations){
      tVector = matrix.map((row) => (row, vector).zipped.map((a,b) => a*b).reduce((a,b) => a+b))
    }
    tVector
  }
  def scalaIterateBFS(iterations: Int,graph:Array[Array[Float]],fringe:Array[Float]) : Array[Float] = {
    var tFringe = fringe
    for(i:Int <- 0 until iterations){
      tFringe = scalaBFSIteration(graph, tFringe)
    }
    tFringe
  }
  def scalaBFSIteration(graph:Array[Array[Float]],fringe:Array[Float]) : Array[Float] = {
    graph.map(
      (row) => (row, fringe).zipped.map((a,b) =>
        if(a>0.0f && b>0.0f) 1.0f else 0.0f
      ).reduce((a,b) =>
        if(a>0.0f || b>0.0f) 1.0f else 0.0f
        )
    )
  }
  def printDFSDotFile(graph:Array[Array[Float]], fringe:Array[Float], gold: Array[Float], init: Array[Float]) : Unit = {
    "pwd"!
    val writer = new PrintWriter(new File("dfsGraph.dot"))
    writer.write("digraph DFSIteration {\n")
    graph.zipWithIndex.map {
      case (row: Array[Float], v1: Int) => row.zipWithIndex.map {
        case (w: Float, v2: Int) => {
//          if (w > 0.0f && (fringe(v1) > 0.0f || fringe(v2) > 0.0f)) {
          if (w > 0.0f) {
            writer.write(v2.toString()+" -> "+v1.toString()+";\n")
          }
        }
      }
    }
    fringe.zipWithIndex.map {
      case (w, v) =>
        if(w>0.0f) {
          writer.write(v.toString() + "[shape=square]\n")
          if(gold(v)<=0.0f)
          {
            writer.write(v.toString()  + "[color=red]\n")
          }
        }
    }
    gold.zipWithIndex.map {
      case (w, v) =>
        if(w>0.0f) {
          if(fringe(v) <=0.0f)
          {
            writer.write(v.toString() + "[shape=triangle]\n")
            writer.write(v.toString() + "[color=red]\n")
          }else{
            writer.write(v.toString() + "[color=green]\n")
          }
        }
    }
    init.zipWithIndex.map {
      case (w, v) =>
        if(w>0.0f) {
          writer.write(v.toString() + "[color=blue]\n")
        }
    }
    writer.write("}\n")
    writer.close()
    "dot -Tpng dfsGraph.dot -odotgraph.png -v -Goverlap=scale"!

    "open dotgraph.png"!

//    dotfileHeader+dotfileBody+dotfileFooter
  }

}
