package openmp.executor

import ir.{ArrayType, TupleType, Type}
import ir.ast.{Get, Split, Transpose, Unzip, UserFun, Zip, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.ir.pattern.{:+, MapPar, ReducePar}

/**
  * Created by Federico on 02-Aug-16.
  */
object Benchmarks {

  val increment = UserFun("increment","x", "return x;", Float,Float)

  def genID(t:Type) = UserFun(s"${t.toString}ID", "x", "return x;", t,t)

  def ultraTrivial(N:Int) = fun(
    ArrayType(Float,N),
    ArrayType(Float,N),
    (inA,inB) => {
      MapSeq(add) $ Zip(inA,inB)
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
    (inA,inB) => {
      toGlobal(MapSeq(id)) o ReducePar(:+(Float), 0.0f) o MapPar(mult) $ Zip(inA,inB)
    }
  )

  def naiveMatrixMult(M:Int, N:Int) = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(ArrayType(Float, N), N),
    (A, B) => {
      toGlobal(MapSeq(MapSeq(MapSeq(id)))) o MapSeq(fun( Arow =>
        MapSeq(fun( Bcol =>
            //ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          toGlobal(MapSeq(id)) o ReduceSeq(add,0.0f) o MapSeq(mult) $ Zip(Arow,Bcol)
        )) o Transpose() $ B
      )) $ A
    })

  def reduceExample(N:Int) = fun(
    ArrayType(Float,N),
    arr => { toGlobal(MapSeq(id)) o ReduceSeq(add,0.0f) $ arr }
  )



  def main(args: Array[String]) {
    val N = 1000
    val ls = (List.iterate(0,N)(x => x + 1)).map(_ => 1)
    val tenList = (List.iterate(0,10)(x => x + 1)).map(_ => (List.iterate(0,10)(x => x + 1).map(_ => 1)))
    val m1 = List(List(1f,0f,0f),List(0f,1f,0f),List(0f,0f,1f))
    val m2 = List(List(1f,1f,1f),List(1f,1f,1f),List(1f,1f,1f))
    Executor.compileAndGenerateScript(dotProductSeq(N),ls ++ ls ++ List(0.0f),"D:/Test")
    //Executor.compileAndGenerateScript(nestedTrivial(10,10),tenList,"D:/Test")
    //Executor.compileAndGenerateScript(naiveMatrixMult(3,3),m1 ++ m2 ++ List(0.0f), "D:/Test")
  }
}
