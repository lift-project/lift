import lift.arithmetic.ArithExpr

package object utils {
  def log(value: ArithExpr, base: ArithExpr): Int = (Math.log(value.evalInt) / Math.log(base.evalInt)).toInt

  // TODO: overload
  def shape1d(seq: Array[Float]): List[Int] = {
    List(seq.size)
  }

  def shape2d(seq: Array[Array[Float]]): List[Int] = {
    List(seq.size) ++ shape1d(seq.head)
  }

  def shape3d(seq: Array[Array[Array[Float]]]): List[Int] = {
    List(seq.size) ++ shape2d(seq.head)
  }

  def shape4d(seq: Array[Array[Array[Array[Float]]]]): List[Int] = {
    List(seq.size) ++ shape3d(seq.head)
  }

  def shape5d(seq: Array[Array[Array[Array[Array[Float]]]]]): List[Int] = {
    List(seq.size) ++ shape4d(seq.head)
  }

  def shape6d(seq: Array[Array[Array[Array[Array[Array[Float]]]]]]): List[Int] = {
    List(seq.size) ++ shape5d(seq.head)
  }
}
