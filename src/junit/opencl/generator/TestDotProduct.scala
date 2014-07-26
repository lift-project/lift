package junit.opencl.generator

import org.junit.Test
import opencl.ir._
import ir._

class TestDotProduct {

  implicit def IntToCst(cst: Int): Cst = new Cst(cst) // try to get this away from here ...

  val id = UserFun("id", Array("x"), "{ return x; }", Int, Int)

  val sumUp = UserFun("sumUp", Array("x", "y"), "{ return x+y; }", TupleType(Int, Int), Int)

  val mult = UserFun("mult", Array("l", "r"), "{ return l * r; }", TupleType(Int, Int), Int)

  val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
                             "{ return acc + (l * r); }",
                             TupleType(Int, TupleType(Int, Int)), Int)

  val N = Cst(1048576)
  // Var("N")
  val left = Input(Var("left"), ArrayType(Int, N))
  val right = Input(Var("right"), ArrayType(Int, N))

  @Test def DOT_PRODUCT_SIMPLE() {

    val kernel = Join() o Join() o MapWrg(
      MapLcl(ReduceSeq(sumUp) o MapSeq(mult))
    ) o Split(4) o Split(128) o Zip(left, right)

  }

  @Test def DOT_PRODUCT_CPU() {

    val firstKernel = Join() o Join() o MapWrg(
      toGlobal(MapLcl(ReduceSeq(multAndSumUp)))
    ) o Split(128) o Split(2048) o Zip(left, right)

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 2048))

    val secondKernel = Join() o MapWrg(
      Join() o MapLcl(ReduceSeq(sumUp)) o Split(8192)
    ) o Split(8192)
  }

  @Test def DOT_PRODUCT() {

    val firstKernel = Join() o Join() o MapWrg(
      toGlobal(MapLcl(ReduceSeq(multAndSumUp))) o ReorderStride()
    ) o Split(128) o Split(2048) o Zip(left, right)

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 2048))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(128)
    ) o Split(8192) o tmp

  }

}