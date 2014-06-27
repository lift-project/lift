package junit

import _root_.opencl.ir.Int
import ir._
import org.junit.Test


class TestType {

  val len = TypeVar()
  val divArrHalf = UserFun("divArrHalf", "empty body", ArrayType(Int, len), ArrayType(Int, len/2))

  @Test def testIterate() {
    val it = new Iterate(3, divArrHalf)
    val t = Type.check(it, ArrayType(Int, len))
    println(t)
  }

}
