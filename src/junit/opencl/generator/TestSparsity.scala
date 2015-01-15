package junit.opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._

object TestSparsity {
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


class TestSparsity {
  val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

  val sq = UserFunDef("sq", "x", "{return x*x;}", Float, Float)

  @Test def MATRIX_SCALAR_MAP() {
    val Msize = 16
    val Nsize = 16
    val matrix = Array.tabulate(Msize,Nsize)(((r,c)=> (r + (c*Nsize)*1.0f)))

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      (A) => {
        MapWrg(fun( Arow =>

          MapLcl( fun( E => sq o E)
          ) o Arow
        )) o A
      }
    )
    val (output, runtime) = Execute(Msize * Nsize)(f, matrix, Nsize, Msize)
    println(matrix.deep.mkString("\n"))
    println(output.deep.mkString("\n"))
  }

  @Test def SPARSE_VECTOR_DOT_PRODUCT() {

    val sum = UserFunDef(
      "sum",
      Array("acc","v"),
      "return (acc+v._1);",
      Seq(Int,TupleType(Int,Int)),
      Int
    )

    val select = UserFunDef(
      "select",
      Array("acc", "v"),
      "{return (v._1 == 0)?acc:v;}",
      Seq(TupleType(Int,Int), TupleType(Int,Int)),
      TupleType(Int,Int)
    )

    val eqMult = UserFunDef(
      "eqMult",
      Array("a","b"),
      "{if(a._0 == b._0){ a._1 = (a._1)*(b._1); }else{ a._1 = 0; } return a;}",
      Array(TupleType(Int,Int),TupleType(Int,Int)),
      TupleType(Int,Int)
    )

    val vectA = List((1, 9),(3, 3),(8, 4),(16,7),(19,1),(20,2),(21,3)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vectB = List((1, 9),(2, 3),(8, 4),(15,7),(19,1),(20,2),(22,3)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vSize = 7

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Int, N),  //first list
      ArrayType(Int, N),  //second list
      (A,B) => {
        ReduceSeq(sum, 0) o MapWrg( //map across the first list
          fun(aElem =>
            ReduceSeq(select, 0.0f) o //reduce to find ones which are "true"
              MapLcl( //map across the first list
                fun(bElem => eqMult(aElem, bElem)) //produce a new list of equational values
              ) o B //map across the second list
          )) o A //map across the first list
      }
    )

    val (output, runtime) = Execute(vSize)(f, vectA, vectB, vSize*2)
    println(output.deep.mkString)
  }

  @Test def DENSE_VECTOR_THING(){

    val add = UserFunDef("add", Array("a","b"), "{return (a+b);}", TupleType(Float, Float), Float)
    val sum = UserFunDef("sum", Array("acc", "v"), "{return acc+v;}", Seq(Float, Float), Float)

    val vect = Array.tabulate(4096)((x:Int) => (x.toFloat)/4096.0f + (1.0f/4096.0f))

    val N = SizeVar("N")
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (A,B) => {
        //          Join() o ReduceSeq(sum, 0.0f) o Split(4) o Zip(A,B)
        Join() o MapWrg(
          Join() o MapLcl(MapSeq(
            fun( pair => add(pair) )
          )) o Split(8)
        ) o Split(512) o Zip(A,B)
      }
    )

    val (output, runtime) = Execute(vect.length)(f, vect, vect.reverse, vect.length)
    println(output.deep.mkString(" "))
  }

  @Test def MATRIX_MAP() {
    val Msize = 16
    val Nsize = 16
    val matrix = Array.tabulate(Msize, Nsize)(((r,c)=> (r+ (c*Nsize)*1.0f)))

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      MatrixType(Float, N,M),
      (m) => {
        MapWrg( fun( row => //for MapWrg, map over rows by default - i.e. feed a row to the next function
          MapLcl( fun( E=> sq o E)
          ) o row
        )) o m
      }
    )
    val CLCode = Compile(f)
    val (output, runtime) = Execute(Msize*Nsize)(f,matrix, Nsize, Msize)
    //    println(matrix.deep.mkString("\n"))
    //    println(output.deep.mkString("\n"))

  }

  @Test def CUST_MATRIX_MAP() {
    val Msize = 8
    val Nsize = 32
    val matrix = Array.tabulate(Msize, Nsize)(((c,r)=> (r+ (c*Nsize)*1.0f)))

    //val N = SizeVar("N")
    //val M = SizeVar("M")

    val f = fun(
      //MatrixType(Float, N,M),
      MatrixType(Float,8,32),
      (m) => {
        //MapWrg ( fun( matrix =>
        MapMatrix( fun( E=> sq o E)
        ) o m
        //)) o m
      }
    )
    //val CLCode = Compile(f)
    val (output, runtime) = Execute(Msize*Nsize)(f,matrix)
    println(matrix.deep.mkString("\n"))
    println(output.deep.mkString("\n"))
  }

}
