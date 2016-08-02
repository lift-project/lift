package openmp.executor

import ir.ArrayType
import ir.ast.{UserFun, Zip, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.ir.pattern.{:+, MapPar, ReducePar}

/**
  * Created by Federico on 02-Aug-16.
  */
object Benchmarks {

  val increment = UserFun("increment","x", "return x;", Float,Float)

  def ultraTrivial(N:Int) = fun(
    ArrayType(Float,N),
    ArrayType(Float,N),
    (inA,inB) => {
      toGlobal(MapSeq(id)) o MapSeq(add) $ Zip(inA,inB)
    }
  )

  def nestedTrivial(N:Int,M:Int) = fun(
    ArrayType(ArrayType(Float,N),M),
    (inA) => {
      toGlobal(MapSeq(MapSeq(increment))) $ inA
    }
  )

  //Sequential dot product
  def dotProductSeq(N:Int) = fun(
    ArrayType(Float,N),
    ArrayType(Float,N),
    Float,
    (inA,inB, init) => {
      toGlobal(MapSeq(id)) o ReduceSeq(add, init) o MapSeq(mult) $ Zip(inA,inB)
    }
  )
  //Parallel dot product
  def dotProductPar(N:Int) = fun(
    ArrayType(Float,N),
    ArrayType(Float,N),
    Float,
    (inA,inB, init) => {
      toGlobal(MapSeq(id))  o ReducePar(:+(Float), init) o MapPar(mult) $ Zip(inA,inB)
    }
  )



  def main(args: Array[String]) {
    val N = 1000
    val ls = (List.iterate(0,N)(x => x + 1)).map(_ => 1)
    val tenList = (List.iterate(0,10)(x => x + 1)).map(_ => (List.iterate(0,10)(x => x + 1).map(_ => 1)))
    //Executor.compileAndGenerateScript(ultraTrivial(N),ls ++ ls ++ List(0.0f),"D:/Test")
    Executor.compileAndGenerateScript(nestedTrivial(10,10),tenList,"D:/Test")
  }
}
