package cbackends.sdh


//combine general IR and backend IR by multiple imports
import cbackends.sdh.sdh_ir._
import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.{Get, Join, Split, UserFun, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, _}
import org.junit.{Ignore, Test}

import scala.sys.process._

class TestSDH {

  val common_path = System.getProperty("user.dir") + "/src/test/cbackends/sdh"

  private def incrementF = fun(Float, x => add(Float).apply(1f, x))

  val N = SizeVar("N")


  /*

  @Test
  def test_vec_scale(): Unit = {

    val path = System.getProperty("user.dir") + "/src/test/sdh/1.vector_scale"

    val f = fun(
      ArrayType(Float, N),
      in => ToLCP() o Join() o  MapTile( Join() o MapGPESync() o MapGPE( TMKernel( MapSeq(incrementF) ) ) o Split(2)  ) o Split(8) o  ToGPE()  $ in
    )
    CompileSDH(f, path)
    //test()
  }

  @Test
  def test_vec_add(): Unit = {

    val path = System.getProperty("user.dir") + "/src/test/sdh/2.vector_add"


    val add2 = UserFun("add", Array("l", "r"),
      "{ return (l + r); }",
      Seq(Float, Float), Float)


    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => ToLCP() o Join() o  MapTile( Join() o MapGPESync() o
        MapGPE( TMKernel( MapSeq( fun(y => add2.apply(Get(y, 0), Get(y, 1))) ) ) ) o Split(2)  ) o Split(8) o  ToGPE()  $ Zip(left, right)
    )
    CompileSDH(f, path)
    //test()
  }
  */

  @Ignore
  @Test
  def test_vec_add_multi_tile(): Unit = {

    val path = s"$common_path/01.vector_add_multi_tile"
    val sched_file = "lib_sched.cpp"
    val worker_file = "test_worker.cpp"


    val add2 = UserFun("add", Array("l", "r"),
      "{ return (l + r); }",
      Seq(Float, Float), Float)


    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),


      (left, right) => ToLCP() o Join() o MapTM( Join() o MapTile( Join() o MapGPESync() o
        MapGPE( TMKernel( MapSeq( fun(y => add2.apply(Get(y, 0), Get(y, 1))) ) ) ) o Split(2) ) o Split(8) ) o Split(16) o ToGPE() $ Zip(left, right)

    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

  }

  @Ignore
  @Test
  def test_matrix_mul_multi_tile_naive(): Unit = {

    //Naive version: the matrix shape has to be 2 x C and C x 4,
    //               to match the 2 tiles and 4 GPEs in a tile

    val path = s"$common_path/02.matrix_mul_multi_tile_naive"
    val sched_file = "test_lift_matrixmul_sched_lib.hpp"
    val worker_file = "test_lift_matrixmul_kernel.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    //only split on A, which is wrong, as it leads to one extra loop on B anyway, thus each thread produce more elements as expected
    /*val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o Join() o
          MapTM(
            Join() o MapTile(
                      Join() o MapGPESync() o MapGPE( TMKernel(
                        MapSeq(
                          fun( Arow => Join() o
                               MapSeq( fun( Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol) ) ) $ B )
                          )
                                    ) ) o Split(2)
                 ) o Split(8)
               ) o Split(16) o ToGPE() $ A
      )*/

    //now the improved version:
    // * MapTM is only a preparation step for A
    // * MapTile map over A
    // * MapGPE map over B
    // * then only a reduce is needed in the kernel, which is executed by each GPE.
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o Join() o MapTM(
          Join() o MapTile( fun(Arow =>
            MapGPESync() o MapGPE( TMKernel(
              fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
            )) $ B )
          ) ) o Split(2) o ToGPE() $ A
    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

  }

  @Test
  def test_matrix_mul_multi_tile_multiples_of_4_for_B(): Unit = {

    val path = s"$common_path/03.matrix_mul_multi_tile_multiples_of_4_for_B"
    val sched_file = "test_lift_matrixmul_sched_lib.hpp"
    val worker_file = "test_lift_matrixmul_kernel.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o Join() o MapTM(
          Join() o MapTile( fun( Arow =>
              MapGPE( TMKernel(
              fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
            )) $ B )
          ) ) o Split(2) o ToGPE() $ A
    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

  }

  @Test
  def test_matrix_mul_sdh_demo(): Unit = {

    val path = s"$common_path/04.matrix_mul_abitrary_size_for_A_B"
    val sched_file = "test_lift_matrixmul_sched_lib.hpp"
    val worker_file = "test_lift_matrixmul_kernel.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o MapTile( fun( Arow =>
            MapGPE( TMKernel(
              fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
            )) $ B )
          )  o ToGPE() $ A
    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

    (s"$path/sdh_demo.sh" ) !

    println("done")
  }







  //( "gedit " + s"$path/$sched_file #&"  ) !
  //val process = Process( "gedit " + s"$path/$sched_file #&" ).lines

  //( "gedit " + s"$path/$sched_file #&; " +  s" gedit $path/$worker_file"  ) !!
  //( s" gedit $path/$worker_file"  ) !!

  /*
  val thread = new Thread {
      override def run {
          // your custom behavior here
        ( "gedit " + s"$path/$sched_file"  ) !!
      }
  }
  thread.start


  Thread.sleep(50)
  */

  @Test
  def test_numpy_sum(): Unit = {

    val path = s"$common_path/05.numpy_sum"
    val sched_file = "test_lift_numpy_sum_sched_lib.hpp"
    val worker_file = "test_lift_numpy_sum_kernel.cpp"

    val N = SizeVar("N")

    /*
    val f = fun(
      ArrayTypeWSWC(Float, N),
      A =>
        ToLCP() o MapTile( fun( Arow =>
          MapGPE( TMKernel(
            fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
          )) $ B )
        )  o ToGPE() $ A
    )*/

    //SDHCompiler ! (f, path, List(sched_file, worker_file))

    println("done")

  }



}
