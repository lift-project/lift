package ir.ast

import ir.{ArrayTypeWSWC, Type, TypeException}
import ir.interpreter.Interpreter.ValueMap
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class PadConstant(left: Int, right: Int, constant: Value)
  extends Pattern(arity = 1) {

  override def toString: String = "PadConstant(" + left + "," + right + "," + constant + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(et,s,c) => ArrayTypeWSWC(et, s + left + right, c + left + right)
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case _: Vector[_] => throw new NotImplementedException()
    }
  }

  def eval2d(arg: Array[Array[Array[Array[Float]]]], padFunc: Int, padOptTotal: Int): Array[Array[Array[Array[Float]]]] = {

    val argPadded = Array.fill(
      arg.length,
      arg.head.length + 2 * padFunc + padOptTotal,
      arg.head.head.length + 2 * padFunc + padOptTotal,
      arg.head.head.head.length)(0.0f)

    for {i <- arg.indices}
      for {j <- arg.head.indices}
        for {k <- arg.head.head.indices}
          for {l <- arg.head.head.head.indices}
            argPadded(i)(j + padFunc)(k + padFunc)(l) = arg(i)(j)(k)(l)

    argPadded
  }

  def evalDepad2d(arg: Array[Array[Array[Array[Float]]]], depadLength: Int): Array[Array[Array[Array[Float]]]] = {

    val argPadded = Array.fill(
      arg.length,
      arg.head.length,
      arg.head.head.length - depadLength,
      arg.head.head.head.length  - depadLength)(0.0f)

    for {i <- argPadded.indices}
      for {j <- argPadded.head.indices}
        for {k <- argPadded.head.head.indices}
          for {l <- argPadded.head.head.head.indices}
            argPadded(i)(j)(k)(l) = arg(i)(j)(k)(l)

    argPadded
  }
}

object PadConstant2D {
   def apply(top: Int, bottom: Int, left: Int, right: Int, constant: Value): Lambda = {
    Map(PadConstant(left, right, constant)) o PadConstant(top, bottom, constant)
  }

  def apply(left: Int, right: Int, constant: Value): Lambda = {
    Map(PadConstant(left, right, constant)) o PadConstant(left, right, constant)
  }

}

object PadConstant3D {
  def apply(x: Int, y: Int, z: Int, constant: Value): Lambda = {
    Map(Map(PadConstant(x, x, constant)) o PadConstant(y, y, constant)) o PadConstant(z, z, constant)
  }
}
