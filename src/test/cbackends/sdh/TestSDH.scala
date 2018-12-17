package cbackends.sdh


//combine general IR and backend IR by multiple imports
import cbackends.sdh.sdh_ir._
import ir.ArrayType
import ir.ast.{Get, Join, Split, UserFun, Zip, fun}

import lift.arithmetic.SizeVar
import opencl.ir.pattern.MapSeq
import opencl.ir.{Float, add, _}
import org.junit.Test

class TestSDH {


  private def incrementF = fun(Float, x => add(Float).apply(1f, x))

  val N = SizeVar("N")


  /*

  @Test
  def test_vec_scale(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/sdh/1.vector_scale"

    val f = fun(
      ArrayType(Float, N),
      in => ToLCP() o Join() o  MapTile( Join() o MapGPESync() o MapGPE( TMKernel( MapSeq(incrementF) ) ) o Split(2)  ) o Split(8) o  ToGPE()  $ in
    )
    CompileSDH(f, path)
    //test()
  }

  @Test
  def test_vec_add(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/sdh/2.vector_add"


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

  @Test
  def test_vec_add_multi_tile(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/sdh/3.vector_add_multi_tile"
    val file = "libvec_add_multi_tile.cpp"


    val add2 = UserFun("add", Array("l", "r"),
      "{ return (l + r); }",
      Seq(Float, Float), Float)


    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),


      (left, right) => ToLCP() o Join() o MapTM( Join() o MapTile( Join() o MapGPESync() o
        MapGPE( TMKernel( MapSeq( fun(y => add2.apply(Get(y, 0), Get(y, 1))) ) ) ) o Split(2) ) o Split(8) ) o Split(16) o ToGPE() $ Zip(left, right)

    )

    SDHCompiler ! (f, path, file)

  }

}